;;; youtube-context-test.el --- Tests for asynchronous YouTube context -*- lexical-binding: t; -*-

(require 'ert)
(require 'youtube-context)

(ert-deftest ai/youtube-context-starts-without-blocking-and-copies-packet ()
  (let* ((script (make-temp-file "youtube-context-fixture-"))
         (ai/youtube-context-program script)
         (ai/youtube-context-process nil)
         (packet
          "Title: Fixture video\nURL: https://www.youtube.com/watch?v=fixture\nTranscript source: video captions\n\nTranscript:\nHello world")
         process)
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/usr/bin/env bash\n"
                    "sleep 0.2\n"
                    "printf '%s' '"
                    packet
                    "'\n"))
          (set-file-modes script #o755)
          (ai/youtube-context "https://www.youtube.com/watch?v=fixture")
          (setq process ai/youtube-context-process)
          (should (process-live-p process))
          (while ai/youtube-context-process
            (accept-process-output process 0.05))
          (should (equal (current-kill 0 t) packet)))
      (when (process-live-p process)
        (delete-process process))
      (when (file-exists-p script)
        (delete-file script)))))

(provide 'youtube-context-test)
;;; youtube-context-test.el ends here
