;;; stream-emacs-init.el --- setup for the sandboxed "stream" Doom Emacs -*- lexical-binding: t; -*-

;; Loaded by the stream-emacs bwrap launcher.  Frames must be identifiable
;; as "StreamEmacs" so the Qtile streamer allow-list recognizes them, and
;; the server must be reachable from the host through the shared
;; /tmp/emacs/<uid> socket directory.

(add-to-list 'default-frame-alist '(name . "StreamEmacs"))
(add-to-list 'default-frame-alist '(title . "StreamEmacs"))

(setq server-name "stream")
(setq server-auth-dir (format "/tmp/emacs/%d/stream-auth" (user-uid)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors (server-start))))

(provide 'stream-emacs-init)
;;; stream-emacs-init.el ends here
