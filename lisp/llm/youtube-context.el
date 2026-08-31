;;; youtube-context.el --- Fetch video context asynchronously -*- lexical-binding: t; -*-

;;; Commentary:
;; Start the canonical youtube-context executable without blocking Emacs.  The
;; process writes the packet that is copied to the kill ring and clipboard.

;;; Code:

(require 'subr-x)

(defgroup ai/youtube-context nil
  "Video transcript context helpers."
  :group 'applications)

(defcustom ai/youtube-context-program "youtube-context"
  "Executable that returns a normalized video context packet."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-yt-dlp-program "yt-dlp"
  "yt-dlp executable passed to the context helper."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-sub-langs "en.*,en"
  "Subtitle language selector passed to yt-dlp."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-program "whisper"
  "Whisper executable used when captions are unavailable."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-model "base.en"
  "Whisper model used for local English transcription."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-language "en"
  "Language passed to Whisper for local transcription."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-threads 16
  "CPU threads passed to Whisper during local transcription."
  :type 'integer
  :group 'ai/youtube-context)

(defvar ai/youtube-context-process nil
  "Currently running YouTube context process, if any.")

(defun ai/youtube--clipboard-text ()
  "Return text from the system clipboard or current kill."
  (or (and (fboundp 'gui-get-selection)
           (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)))
      (ignore-errors (current-kill 0 t))))

(defun ai/youtube--clipboard-url ()
  "Return an HTTP URL from the clipboard, or nil."
  (let ((text (ai/youtube--clipboard-text)))
    (when text
      (let ((url (string-trim text)))
        (when (string-match-p "\\`https?://" url)
          url)))))

(defun ai/youtube--configured-environment ()
  "Return the helper environment from the current YouTube settings."
  (let ((environment (copy-sequence process-environment)))
    (dolist (setting
             `(("YOUTUBE_CONTEXT_YT_DLP_PROGRAM"
                . ,ai/youtube-context-yt-dlp-program)
               ("YOUTUBE_CONTEXT_SUB_LANGS" . ,ai/youtube-context-sub-langs)
               ("YOUTUBE_CONTEXT_WHISPER_PROGRAM"
                . ,ai/youtube-context-whisper-program)
               ("YOUTUBE_CONTEXT_WHISPER_MODEL"
                . ,ai/youtube-context-whisper-model)
               ("YOUTUBE_CONTEXT_WHISPER_LANGUAGE"
                . ,ai/youtube-context-whisper-language)
               ("YOUTUBE_CONTEXT_WHISPER_THREADS"
                . ,(number-to-string ai/youtube-context-whisper-threads))))
      (setenv (car setting) (cdr setting)))
    environment))

(defun ai/youtube--process-buffer-string (buffer)
  "Return BUFFER contents without text properties."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun ai/youtube--copy-context (context)
  "Copy a valid CONTEXT packet and report its metadata."
  (unless (string-match
           "\\`Title: \\(.*\\)\nURL: .*\nTranscript source: \\(.*\\)\n\nTranscript:\n\\(.*\\)\\'"
           context)
    (error "youtube-context returned an invalid context packet"))
  (let* ((title (match-string 1 context))
         (source (match-string 2 context))
         (transcript (match-string 3 context))
         (words (length (split-string transcript "[[:space:]]+" t))))
    (kill-new context)
    (when (fboundp 'gui-set-selection)
      (ignore-errors (gui-set-selection 'CLIPBOARD context)))
    (message "Copied YouTube context: %d words from %s via %s"
             words title source)))

(defun ai/youtube--process-sentinel (process _event)
  "Copy PROCESS output after its asynchronous command finishes."
  (when (memq (process-status process) '(exit signal))
    (let* ((output-buffer (process-buffer process))
           (error-buffer (process-get process 'ai/youtube-context-error-buffer))
           (status (process-exit-status process))
           (context (ai/youtube--process-buffer-string output-buffer))
           (error-text (string-trim
                        (ai/youtube--process-buffer-string error-buffer))))
      (unwind-protect
          (if (and (eq (process-status process) 'exit) (= status 0))
              (condition-case error
                  (ai/youtube--copy-context context)
                (error
                 (message "YouTube context failed: %s"
                          (error-message-string error))))
            (message "YouTube context failed%s"
                     (if (string-empty-p error-text)
                         (format " with status %d" status)
                       (format ": %s" error-text))))
        (when (eq process ai/youtube-context-process)
          (setq ai/youtube-context-process nil))
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer))
        (when (buffer-live-p error-buffer)
          (kill-buffer error-buffer))))))

;;;###autoload
(defun ai/youtube-context (url)
  "Fetch a transcript context packet for URL without blocking Emacs.

The canonical youtube-context executable prefers English captions and falls
back to local Whisper transcription.  Its completed output is copied to the
kill ring and system clipboard.  The prompt defaults to an HTTP URL in the
clipboard."
  (interactive
   (list
    (read-string "Video URL: " (or (ai/youtube--clipboard-url) ""))))
  (when (process-live-p ai/youtube-context-process)
    (user-error "A YouTube context request is already running"))
  (unless (string-match-p "\\`https?://" url)
    (user-error "Expected an http(s) video URL"))
  (let ((program (if (file-name-directory ai/youtube-context-program)
                     (and (file-executable-p ai/youtube-context-program)
                          ai/youtube-context-program)
                   (executable-find ai/youtube-context-program))))
    (unless program
      (user-error "%s is not available in PATH" ai/youtube-context-program))
    (let ((output-buffer (generate-new-buffer " *youtube-context-output*"))
          (error-buffer (generate-new-buffer " *youtube-context-errors*"))
          process)
      (condition-case error
          (let ((process-environment (ai/youtube--configured-environment)))
            (setq process
                  (make-process
                   :name (buffer-name output-buffer)
                   :buffer output-buffer
                   :stderr error-buffer
                   :command (list program url)
                   :noquery t
                   :connection-type 'pipe
                   :sentinel #'ai/youtube--process-sentinel))
            (process-put process 'ai/youtube-context-error-buffer error-buffer)
            (setq ai/youtube-context-process process)
            (message "Fetching YouTube context asynchronously..."))
        (error
         (kill-buffer output-buffer)
         (kill-buffer error-buffer)
         (signal (car error) (cdr error)))))))

(provide 'youtube-context)
;;; youtube-context.el ends here
