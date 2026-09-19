;;; gpt-todos.el -*- lexical-binding: t; -*-

(defcustom gpt-todos-sync-script
  (or (getenv "GPT_TODOS_SYNC")
      (expand-file-name "~/.dotfiles/scripts/gpt-todos-sync"))
  "Path to the dotfiles-owned gpt-todos sync script."
  :type 'file)

(defcustom gpt-todos-notes-directory
  (file-name-as-directory
   (or (getenv "GPT_TODOS_NOTES_DIR")
       (expand-file-name "~/Documents/Notes/org")))
  "Live full Org workspace mirrored by gpt-todos-sync."
  :type 'directory)

(defcustom gpt-todos-agenda-directory
  (file-name-as-directory
   (or (getenv "GPT_TODOS_ORG_DIR")
       (expand-file-name "agenda" gpt-todos-notes-directory)))
  "Live agenda subtree kept compatible with Todo Manager."
  :type 'directory)

(defcustom gpt-todos-roam-db-location
  (expand-file-name
   "org-roam/gpt-todos.db"
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (expand-file-name "~/.cache"))))
  "Derived local Org-roam database for the full gpt-todos notes graph."
  :type 'file)

(defun gpt-todos--synced-org-file-p (file)
  "Return non-nil when FILE is an Org file inside the synchronized workspace."
  (when file
    (let ((root (file-name-as-directory
                 (expand-file-name gpt-todos-notes-directory)))
          (candidate (expand-file-name file)))
      (and (string-equal (file-name-extension candidate) "org")
           (string-prefix-p root candidate)))))

(defun gpt-todos-org-files (&optional directory)
  "Return all Org files recursively below DIRECTORY or the full notes root."
  (directory-files-recursively
   (file-name-as-directory
    (expand-file-name (or directory gpt-todos-notes-directory)))
   "\\.org\\'"))

(defun gpt-todos-configure-org-roam ()
  "Use the full synchronized notes tree as the canonical Org-roam graph."
  (make-directory (file-name-directory gpt-todos-roam-db-location) t)
  (setq org-roam-directory
        (file-truename
         (file-name-as-directory
          (expand-file-name gpt-todos-notes-directory)))
        org-roam-dailies-directory "daily"
        org-roam-db-location gpt-todos-roam-db-location
        org-roam-complete-everywhere t))

(with-eval-after-load 'org-roam
  (gpt-todos-configure-org-roam))

;;;###autoload
(defun gpt-todos-org-ql-shared-memory ()
  "Open an Org QL view of active shared agent-memory assertions."
  (interactive)
  (require 'org-ql)
  (org-ql-search
   (gpt-todos-org-files)
   '(and (property "KIND" "memory")
         (property "MEMORY_SCOPE" "shared")
         (property "STATUS" "active"))
   :title "Shared Agent Memory"))

;;;###autoload
(defun gpt-todos-org-ql-inventory ()
  "Open an Org QL view of inventory entities and events."
  (interactive)
  (require 'org-ql)
  (org-ql-search
   (gpt-todos-org-files)
   '(or (property "KIND" "inventory-item")
        (property "KIND" "inventory-location")
        (property "KIND" "inventory-event")
        (property "KIND" "food-event"))
   :title "Inventory / Food"))

;;;###autoload
(defun gpt-todos-sync (&optional file)
  "Run gpt-todos sync asynchronously.
When FILE is non-nil, preserve and synchronize that just-saved Org file."
  (interactive)
  (let* ((buffer (get-buffer-create " *gpt-todos-sync*"))
         (command (append (list "/usr/bin/env" "bash" gpt-todos-sync-script)
                          (when file
                            (list "--file" (expand-file-name file)))))
         (proc (make-process
                :name "gpt-todos-sync"
                :buffer buffer
                :command command
                :connection-type 'pipe
                :noquery t)))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (if (= 0 (process-exit-status p))
             (message "gpt-todos sync complete")
           (message "gpt-todos sync failed; see %s"
                    (buffer-name (process-buffer p)))))))
    (message "gpt-todos sync started")
    proc))

;;;###autoload
(defun gpt-todos-sync-after-save ()
  "Synchronize a saved Org file when it belongs to the full notes workspace."
  (when (gpt-todos--synced-org-file-p buffer-file-name)
    (gpt-todos-sync buffer-file-name)))

;;;###autoload
(add-hook 'after-save-hook #'gpt-todos-sync-after-save)
