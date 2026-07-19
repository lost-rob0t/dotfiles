;;; Chemacs loader


(defun chemacs-read-profile-name (prompt  &optional predicate require-match initial-input hist def inherit-input-system)
  (completing-read prompt (mapcar #'car (read (with-temp-buffer (insert-file-contents (expand-file-name ".emacs-profiles.el" (getenv "HOME")))))) predicate require-match initial-input hist def inherit-input-system))

(defun nsa/switch-emacs (&optional arg)
  "Interactivly select a chemacs instance to use, select that version and exit"
  (let (())))
