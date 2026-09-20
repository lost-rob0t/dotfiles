;;; ai-roam-vector.el --- org-vector semantic search gptel tools -*- lexical-binding: t; -*-

;;; Commentary:

;; org-vector semantic search and indexing for the roam AI:
;;
;; - `ai/roam-vector-search' queries the org-vector embeddings for the
;;   roam graph and returns the tool's raw stdout.
;; - `ai/roam-vector-embed' refreshes the embeddings.  It writes only
;;   to the vector store, never to note content, so no roam editor
;;   rights gate applies to either tool.
;; - `ai/roam-vector-register-gptel-tools' exposes the synchronous
;;   `search_notes_semantic' and `index_notes_embeddings' gptel tools.
;;   Both fail soft: they return error strings instead of signaling,
;;   so gptel tool results stay clean.
;;
;; The CLI is resolved with `executable-find' at call time and defaults
;; to the Home Manager-installed `org-vector' binary.  Every directory
;; default resolves lazily so tests can bind the variables.
;;
;; This module is Doom-free so it can be tested in batch Emacs.

;;; Code:

(require 'ai-roam)
(require 'cl-lib)
(require 'subr-x)

(declare-function gptel-get-tool "gptel" (name))
(declare-function gptel-make-tool "gptel" (&rest slots))

(defgroup ai-roam-vector nil
  "org-vector semantic tools for the roam AI."
  :group 'ai-roam)

(defconst ai/roam-vector-gptel-tool-search "search_notes_semantic"
  "Name of the gptel tool that queries the org-vector index.")

(defconst ai/roam-vector-gptel-tool-index "index_notes_embeddings"
  "Name of the gptel tool that refreshes the org-vector index.")

(defcustom ai/roam-vector-command "org-vector"
  "org-vector executable name or path, resolved at call time."
  :type 'string
  :group 'ai-roam-vector)

(defcustom ai/roam-vector-org-dir nil
  "Directory org-vector indexes.
When nil, falls back to the expansion of `ai/roam-directory',
resolved at call time so rebinding the roam root rebinds the index."
  :type '(choice string (const nil))
  :group 'ai-roam-vector)

(defcustom ai/roam-vector-db "~/.cache/org-vector/"
  "Directory holding the org-vector embedding store."
  :type 'string
  :group 'ai-roam-vector)

(defcustom ai/roam-vector-top-k 8
  "Number of results a semantic search returns."
  :type 'integer
  :group 'ai-roam-vector)

(defun ai/roam-vector--org-dir ()
  "Return the directory org-vector indexes, expanded at call time."
  (file-name-as-directory
   (expand-file-name
    (or ai/roam-vector-org-dir (ai/roam--directory)))))

(defun ai/roam-vector--db ()
  "Return the expanded org-vector embedding store path."
  (file-name-as-directory (expand-file-name ai/roam-vector-db)))

(defun ai/roam-vector--args (subcommand &rest extra)
  "Return the org-vector argument list for SUBCOMMAND plus EXTRA."
  (cons subcommand extra))

(defun ai/roam-vector--search-args (query)
  "Return the org-vector arguments searching the index for QUERY."
  (ai/roam-vector--args
   "search"
   "--query" query
   "--dir" (ai/roam-vector--org-dir)
   "--path" (ai/roam-vector--db)
   "--results" (number-to-string ai/roam-vector-top-k)))

(defun ai/roam-vector--embed-args ()
  "Return the org-vector arguments refreshing the index."
  (ai/roam-vector--args
   "embed"
   "--dir" (ai/roam-vector--org-dir)
   "--path" (ai/roam-vector--db)))

(defun ai/roam-vector--run (subcommand &rest extra)
  "Run `ai/roam-vector-command' SUBCOMMAND with EXTRA arguments.
Synchronous `call-process'.  Return the trimmed stdout on success,
or nil when the executable is missing or exits non-zero."
  (let ((command (executable-find ai/roam-vector-command t)))
    (when command
      (with-temp-buffer
        (let ((status (apply #'call-process command nil t nil
                             (apply #'ai/roam-vector--args
                                    subcommand extra))))
          (when (and (numberp status) (= status 0))
            (string-trim (buffer-string))))))))

(defun ai/roam-vector-search (query)
  "Search the org-vector index for QUERY.
Read-only: queries the vector store and never writes note content,
so no roam editor-rights gate applies.  Return the trimmed stdout
on success; on failure return an error string starting with
\"org-vector\"."
  (or (apply #'ai/roam-vector--run (ai/roam-vector--search-args query))
      (format "org-vector: search failed (%s missing or exited non-zero)"
              ai/roam-vector-command)))

(defun ai/roam-vector-embed ()
  "Refresh the org-vector embeddings for the roam graph.
Indexing writes only to the vector store under `ai/roam-vector-db';
note content is never edited, so no roam editor-rights gate applies.
Return the trimmed stdout summary on success; on failure return an
error string starting with \"org-vector\"."
  (or (apply #'ai/roam-vector--run (ai/roam-vector--embed-args))
      (format "org-vector: embed failed (%s missing or exited non-zero)"
              ai/roam-vector-command)))

(defun ai/roam-vector--clear-gptel-tool (name)
  "Remove an existing gptel tool named NAME before re-registering it."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool name) nil))))

(defun ai/roam-vector-register-gptel-tools ()
  "Register the org-vector semantic tools with gptel.
Both tools are synchronous and fail soft: they return error strings
instead of signaling.  Registration is idempotent."
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel is required for org-vector tools"))
  (ai/roam-vector--clear-gptel-tool ai/roam-vector-gptel-tool-search)
  (ai/roam-vector--clear-gptel-tool ai/roam-vector-gptel-tool-index)
  (gptel-make-tool
   :name ai/roam-vector-gptel-tool-search
   :function #'ai/roam-vector-search
   :category "roam"
   :description "Semantic search over the user's org-roam notes with org-vector. QUERY is free text; returns matching note excerpts with their file paths. Read-only."
   :args '((:name "query"
            :type string
            :description "Free-text search over the indexed org-roam notes"))
   :include t)
  (gptel-make-tool
   :name ai/roam-vector-gptel-tool-index
   :function #'ai/roam-vector-embed
   :category "roam"
   :description "Refresh the org-vector embeddings for the user's org-roam notes so semantic search sees recent notes. Writes only to the embedding store; note content is never edited. Call before searching when notes changed recently."
   :args nil
   :include t)
  (when (boundp 'ai/agent-tools)
    (cl-pushnew ai/roam-vector-gptel-tool-search ai/agent-tools :test #'equal)
    (cl-pushnew ai/roam-vector-gptel-tool-index ai/agent-tools :test #'equal))
  (list ai/roam-vector-gptel-tool-search ai/roam-vector-gptel-tool-index))

(provide 'ai-roam-vector)
;;; ai-roam-vector.el ends here
