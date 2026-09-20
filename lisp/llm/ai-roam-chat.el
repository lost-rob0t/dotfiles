;;; ai-roam-chat.el --- Rights-aware roam sidebar chat -*- lexical-binding: t; -*-

;;; Commentary:

;; A rights-aware gptel sidebar chat for the roam AI.
;;
;; `ai/roam-chat' opens a dedicated right side window whose system
;; message states the note's roam section, restates the resolved
;; editor rights in plain words, and names the roam tools with their
;; intended use.  The buffer-local tool list reuses the globally
;; active tools plus the registered roam tools (`create_roam_id_link',
;; `search_notes_semantic', `index_notes_embeddings',
;; `remember_fact', `recall_memory', `assert_world_fact').
;;
;; The pure helpers (`ai/roam-chat--context',
;; `ai/roam-chat--system-message', `ai/roam-chat--tools') are
;; Doom-free and batch-safe: org-roam backlink context is added only
;; when org-roam is loadable and answers, gptel absence makes the
;; tool helper return nil, and `ai/roam-chat' itself user-errors
;; cleanly without gptel.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-roam)
(require 'ai-roam-links)
(require 'ai-roam-vector)
(require 'ai-roam-memory)

(declare-function gptel-get-tool "gptel" (name))
(declare-function gptel-mode "gptel" (&optional arg))
(declare-function ai/llm-backend "ai" (&optional provider no-error))
(declare-function ai/llm-resolve-model "ai" (&optional provider))
(declare-function org-roam-id-at-file "org-roam-id" (file))
(declare-function org-roam-node-from-id "org-roam-node" (id))
(declare-function org-roam-backlinks-get "org-roam-backlinks" (node))
(declare-function org-roam-node-file "org-roam-node" (node))

(defgroup ai-roam-chat nil
  "Rights-aware roam sidebar chat."
  :group 'ai-roam)

(defcustom ai/roam-chat-window-width 0.35
  "Width of the roam sidebar chat window as a fraction of the frame."
  :type 'number
  :group 'ai-roam-chat)

(defun ai/roam-chat--backlinks (file)
  "Return the file names linking to FILE via org-roam, or nil.
Return nil when org-roam is unavailable, its database is not ready,
or FILE has no backlinks; the sidebar context simply omits them."
  (when (require 'org-roam nil t)
    (ignore-errors
      (let* ((id (org-roam-id-at-file file))
             (node (and id (org-roam-node-from-id id)))
             (backlinks (and node (org-roam-backlinks-get node))))
        (when backlinks
          (mapcar #'org-roam-node-file backlinks))))))

(defun ai/roam-chat--context (&optional file)
  "Return the roam chat context plist for FILE.
Keys: :file, :section, and :rights always; :backlinks only when
org-roam is loadable and reports backlinks for FILE.  The org-roam
part stays omitted in batch Emacs so the plist is deterministic
there."
  (let ((context (list :file file
                       :section (ai/roam-section-for-file file)
                       :rights (ai/roam-rights file))))
    (when (and file (plist-get context :section))
      (let ((backlinks (ai/roam-chat--backlinks file)))
        (when backlinks
          (setq context (plist-put context :backlinks backlinks)))))
    context))

(defun ai/roam-chat--describe-section (section)
  "Return plain words for the roam SECTION symbol."
  (if section
      (format "the %s section" section)
    "no specific roam section"))

(defun ai/roam-chat--describe-rights (rights)
  "Return plain words for the RIGHTS symbol."
  (if (eq rights 'full)
      "You have FULL editor rights in this section: you may edit note content and you may create roam:id links when asked."
    "Rights are OUTLINE-ONLY in this section: you may draft org outlines for the user to fill in; you must not edit note content or create roam:id links."))

(defconst ai/roam-chat--tool-instructions
  "Available roam tools:
- search_notes_semantic: semantic search over my notes; use it to find relevant notes before answering questions about their content.
- index_notes_embeddings: refresh the embedding index; use it first when recently written notes are missing from search results.
- create_roam_id_link: prepare a roam:id link target for an existing note; use it only when I ask to link notes and only where the rights statement above allows editing.
Durable memory is available through remember_fact and recall_memory, and assert_world_fact appends structured world facts; both memory writes and structured facts require the editor-rights gate to pass."
  "Tool names and usage rules embedded in the sidebar system message.")

(defun ai/roam-chat--system-message (&optional file)
  "Compose the roam sidebar chat system message for FILE.
The message states the roam section, restates the resolved editor
rights in plain words, and names the roam tools with their intended
use."
  (let ((context (ai/roam-chat--context file)))
    (string-join
     (list (format "You are the roam AI assistant in a sidebar chat about %s."
                   (ai/roam-chat--describe-section
                    (plist-get context :section)))
           (ai/roam-chat--describe-rights (plist-get context :rights))
           ai/roam-chat--tool-instructions)
     "\n\n")))

(defun ai/roam-chat--buffer-name (&optional name file)
  "Return the sidebar chat buffer name for NAME or FILE's section.
An explicit NAME is used verbatim; otherwise the section of FILE
names the buffer, falling back to a generic notes chat."
  (or name
      (format "*roam: %s*"
              (or (ai/roam-section-for-file file) "notes"))))

(defun ai/roam-chat--tools ()
  "Return the gptel tools for the roam sidebar chat.
The list copies the globally active tools and adds the roam tools
when their register functions have run.  Return nil when gptel is
unavailable so the helper stays batch-safe."
  (when (require 'gptel nil t)
    (let ((tools (copy-sequence (default-value 'gptel-tools))))
      (dolist (name (list ai/roam-links-gptel-tool
                          ai/roam-vector-gptel-tool-search
                          ai/roam-vector-gptel-tool-index
                          ai/roam-memory-gptel-tool-remember
                          ai/roam-memory-gptel-tool-recall
                          ai/roam-memory-gptel-tool-assert-fact))
        (when-let* ((tool (gptel-get-tool name)))
          (cl-pushnew tool tools :test #'eq)))
      tools)))

(defun ai/roam-chat--configure-buffer (context)
  "Configure the current buffer as a roam sidebar gptel chat for CONTEXT."
  (org-mode)
  (setq-local gptel-backend (ai/llm-backend))
  (setq-local gptel-model (ai/llm-resolve-model))
  (setq-local gptel-system-message
              (ai/roam-chat--system-message (plist-get context :file)))
  (setq-local gptel-tools (ai/roam-chat--tools))
  (setq-local gptel-use-tools t)
  (setq-local gptel-include-tool-results t)
  (setq-local gptel-use-context 'system)
  (setq-local gptel-track-media t)
  (setq-local gptel-include-reasoning t)
  (setq-local org-startup-with-inline-images t)
  (gptel-mode 1))

;;;###autoload
(defun ai/roam-chat (&optional name)
  "Open a rights-aware roam AI sidebar chat named NAME.
The chat buffer is displayed in a dedicated right side window.  Its
system message states the roam section of the current buffer's file
and the resolved editor rights; its tool set reuses the globally
active tools plus the registered roam tools.  With no NAME the
section of the current buffer's file names the buffer."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel is required for the sidebar chat"))
  (let* ((origin-file (buffer-file-name))
         (context (ai/roam-chat--context origin-file))
         (buffer (or (get-buffer (ai/roam-chat--buffer-name name origin-file))
                     (get-buffer-create
                      (ai/roam-chat--buffer-name name origin-file)))))
    (with-current-buffer buffer
      (ai/roam-chat--configure-buffer context))
    (let ((window (display-buffer
                   buffer
                   `(display-buffer-in-side-window
                     (side . right)
                     (window-width . ,ai/roam-chat-window-width)))))
      (set-window-parameter window 'dedicated t))
    (pop-to-buffer buffer)))

(provide 'ai-roam-chat)
;;; ai-roam-chat.el ends here
