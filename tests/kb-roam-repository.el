;;; kb-roam-repository.el --- Audited repository write tests -*- lexical-binding: t; -*-
(require 'ert)

(defconst kb-test-document ":PROPERTIES:\n:ID: stable-research-id\n:END:\n#+title: Research\n#+filetags: :couchdb:performance:\n\n* Findings\nEvidence.\n")

(defmacro kb-test-root (&rest body)
  `(let* ((root (make-temp-file "kb-repository-" t))
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn (setenv "KB_ROAM_ROOT" root) ,@body)
       (delete-directory root t))))

(ert-deftest kb-repository-create-and-update ()
  (kb-test-root
   (let* ((path (kb-roam-write-document "research/note.org" kb-test-document "test:research" "absent"))
          (old (with-temp-buffer (insert-file-contents path) (buffer-string)))
          (hash (secure-hash 'sha256 old)))
     (should (string-match-p "AI_GENERATED: t" old))
     (should (string-match-p "ROAM_VISIBILITY: private" old))
     (should-error (kb-roam-write-document "research/note.org" kb-test-document "test:again" "absent"))
     (should-error (kb-roam-write-document "research/note.org" kb-test-document "test:again" "bad-hash"))
     (let ((updated (replace-regexp-in-string "Evidence\\." "More evidence." old)))
       (kb-roam-write-document "research/note.org" updated "test:again" hash)
       (with-temp-buffer
         (insert-file-contents path)
         (should (equal (kb-roam--keyword "AI_TAG")
                        (with-temp-buffer (insert old) (kb-roam--keyword "AI_TAG")))))
       (should-error (kb-roam-write-document "research/note.org" updated "test:stale" hash))))))

(ert-deftest kb-repository-rejects-escape-and-symlink ()
  (kb-test-root
   (dolist (path '("../escape.org" "/tmp/escape.org" "note.txt" "research/../../escape.org"))
     (should-error (kb-roam-write-document path kb-test-document "test:bounds" "absent")))
   (make-symbolic-link (temporary-file-directory) (expand-file-name "outside" root))
   (should-error (kb-roam-write-document "outside/escape.org" kb-test-document "test:bounds" "absent"))))

(ert-deftest kb-repository-preserves-identity ()
  (kb-test-root
   (let* ((path (kb-roam-write-document "note.org" kb-test-document "test:identity" "absent"))
          (old (with-temp-buffer (insert-file-contents path) (buffer-string)))
          (hash (secure-hash 'sha256 old)))
     (should-error (kb-roam-write-document "note.org"
                   (replace-regexp-in-string "stable-research-id" "different-id" old)
                   "test:identity" hash))
     (should-error (kb-roam-write-document "note.org"
                   (replace-regexp-in-string "ai_[a-f0-9]+" "ai_0000000000000000" old)
                   "test:identity" hash)))))

(ert-deftest kb-repository-requires-provenance-and-topics ()
  (kb-test-root
   (should-error (kb-roam-write-document "note.org" kb-test-document "" "absent"))
   (should-error (kb-roam-write-document "note.org"
                 (replace-regexp-in-string ":couchdb:performance:" ":note:" kb-test-document)
                 "test:tags" "absent"))))
