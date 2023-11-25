;; This package is random time related utilities i have created.
(defun nsa/insert-unix-epoch ()
  "Inserts the current Unix epoch at point."
  (interactive)
  (insert (number-to-string (floor (time-to-seconds (current-time))))))


(provide 'saved-time.el)
