(defun +gptel/here ()
  "Spawn maximized gptel buffer."
  (interactive)
  (call-interactively #'gptel)
  (doom/window-maximize-buffer))
