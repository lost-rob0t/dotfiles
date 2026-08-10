;;; youtube-context.el --- Copy video transcripts for LLM context -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch English subtitles with yt-dlp, normalize them to plain text, and copy
;; a compact context packet to the kill ring/system clipboard.  When subtitles
;; are unavailable, download the audio and transcribe it locally with Whisper.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup ai/youtube-context nil
  "Video transcript context helpers."
  :group 'applications)

(defcustom ai/youtube-context-program "yt-dlp"
  "yt-dlp executable used to fetch subtitles and audio."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-sub-langs "en.*,en"
  "yt-dlp subtitle language selector used for transcript context."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-program "whisper"
  "Whisper executable used when the video has no usable subtitles."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-model "base.en"
  "Whisper model used for subtitle-free videos."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-language "en"
  "Language passed to Whisper for subtitle-free videos."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-device "cpu"
  "Device passed to Whisper for local transcription."
  :type 'string
  :group 'ai/youtube-context)

(defun ai/youtube--clipboard-text ()
  "Return text from the system clipboard or current kill."
  (or (and (fboundp 'gui-get-selection)
           (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)))
      (ignore-errors (current-kill 0 t))))

(defun ai/youtube--clipboard-url ()
  "Return an HTTP URL from the clipboard, or nil."
  (when-let* ((text (ai/youtube--clipboard-text))
              (url (string-trim text))
              ((string-match-p "\\`https?://" url)))
    url))

(defun ai/youtube--normalize-text (text)
  "Normalize subtitle TEXT into compact plain text."
  (string-trim
   (replace-regexp-in-string
    "[[:space:]]+" " "
    (replace-regexp-in-string "<[^>]+>" "" text))))

(defun ai/youtube--json3-transcript (file)
  "Extract plain transcript text from yt-dlp JSON3 subtitle FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((data (json-parse-buffer
                  :object-type 'hash-table
                  :array-type 'list
                  :null-object nil
                  :false-object nil))
           (events (gethash "events" data))
           chunks
           previous)
      (dolist (event events)
        (let* ((segments (gethash "segs" event))
               (chunk
                (and segments
                     (ai/youtube--normalize-text
                      (mapconcat
                       (lambda (segment)
                         (or (gethash "utf8" segment) ""))
                       segments "")))))
          (when (and chunk
                     (not (string-empty-p chunk))
                     (not (equal chunk previous)))
            (push chunk chunks)
            (setq previous chunk))))
      (ai/youtube--normalize-text
       (string-join (nreverse chunks) " ")))))

(defun ai/youtube--vtt-transcript (file)
  "Extract plain transcript text from WebVTT subtitle FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (lines previous)
      (dolist (line (split-string (buffer-string) "\n"))
        (setq line (string-trim line))
        (unless (or (string-empty-p line)
                    (string-prefix-p "WEBVTT" line)
                    (string-prefix-p "Kind:" line)
                    (string-prefix-p "Language:" line)
                    (string-match-p "-->" line)
                    (string-match-p "\\`[0-9]+\\'" line))
          (setq line (ai/youtube--normalize-text line))
          (when (and (not (string-empty-p line))
                     (not (equal line previous)))
            (push line lines)
            (setq previous line))))
      (ai/youtube--normalize-text
       (string-join (nreverse lines) " ")))))

(defun ai/youtube--subtitle-score (file)
  "Return preference score for subtitle FILE; lower is better."
  (let ((name (file-name-nondirectory file)))
    (+ (if (string-suffix-p ".json3" name) 0 100)
       (cond
        ((string-match-p "\\.en\\.\\(?:json3\\|vtt\\)\\'" name) 0)
        ((string-match-p "\\.en-US\\.\\(?:json3\\|vtt\\)\\'" name) 1)
        ((string-match-p "\\.en-GB\\.\\(?:json3\\|vtt\\)\\'" name) 2)
        ((string-match-p "\\.en[^.]*\\.\\(?:json3\\|vtt\\)\\'" name) 10)
        (t 50)))))

(defun ai/youtube--best-subtitle (directory)
  "Return the preferred downloaded subtitle file in DIRECTORY."
  (car
   (sort
    (directory-files directory t "\\.\\(?:json3\\|vtt\\)\\'")
    (lambda (a b)
      (< (ai/youtube--subtitle-score a)
         (ai/youtube--subtitle-score b))))))

(defun ai/youtube--process-output (buffer)
  "Return trimmed text from process BUFFER."
  (with-current-buffer buffer
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai/youtube--download-subtitle (program url directory)
  "Use PROGRAM to download subtitles for URL into DIRECTORY.
Return the preferred subtitle file, or nil when none are available."
  (let ((default-directory directory)
        (buffer (generate-new-buffer " *yt-dlp-context*")))
    (unwind-protect
        (progn
          (process-file
           program nil buffer nil
           "--skip-download"
           "--no-playlist"
           "--write-subs"
           "--write-auto-subs"
           "--sub-langs" ai/youtube-context-sub-langs
           "--sub-format" "json3/vtt/best"
           "--output" "%(id)s.%(ext)s"
           url)
          (ai/youtube--best-subtitle directory))
      (kill-buffer buffer))))

(defun ai/youtube--download-audio (program url directory)
  "Use PROGRAM to download URL audio into DIRECTORY and return its path."
  (let ((default-directory directory)
        (buffer (generate-new-buffer " *yt-dlp-audio*")))
    (unwind-protect
        (let ((status
               (process-file
                program nil buffer nil
                "--no-playlist"
                "--extract-audio"
                "--audio-format" "wav"
                "--output" "%(id)s.%(ext)s"
                url)))
          (unless (eq status 0)
            (error "yt-dlp audio download failed: %s"
                   (ai/youtube--process-output buffer)))
          (or (car (directory-files directory t "\\.wav\\'"))
              (error "yt-dlp produced no WAV audio file")))
      (kill-buffer buffer))))

(defun ai/youtube--read-text-file (file)
  "Read FILE and return normalized text."
  (with-temp-buffer
    (insert-file-contents file)
    (ai/youtube--normalize-text (buffer-string))))

(defun ai/youtube--whisper-transcript (audio directory)
  "Transcribe AUDIO into DIRECTORY with local Whisper and return plain text."
  (let ((program (executable-find ai/youtube-context-whisper-program)))
    (unless program
      (user-error
       "No subtitles found and %s is not available in PATH"
       ai/youtube-context-whisper-program))
    (let ((default-directory directory)
          (buffer (generate-new-buffer " *youtube-whisper*")))
      (unwind-protect
          (let ((status
                 (process-file
                  program nil buffer nil
                  audio
                  "--model" ai/youtube-context-whisper-model
                  "--language" ai/youtube-context-whisper-language
                  "--device" ai/youtube-context-whisper-device
                  "--fp16" "False"
                  "--verbose" "False"
                  "--output_format" "txt"
                  "--output_dir" directory)))
            (unless (eq status 0)
              (error "Whisper transcription failed: %s"
                     (ai/youtube--process-output buffer)))
            (let ((file
                   (car
                    (sort
                     (directory-files directory t "\\.txt\\'")
                     (lambda (a b)
                       (time-less-p
                        (file-attribute-modification-time
                         (file-attributes b))
                        (file-attribute-modification-time
                         (file-attributes a))))))))
              (unless file
                (error "Whisper produced no transcript text file"))
              (ai/youtube--read-text-file file)))
        (kill-buffer buffer)))))

(defun ai/youtube--transcript (program url directory)
  "Return (TRANSCRIPT . SOURCE) for URL using PROGRAM and DIRECTORY."
  (if-let ((subtitle (ai/youtube--download-subtitle program url directory)))
      (cons
       (if (string-suffix-p ".json3" subtitle)
           (ai/youtube--json3-transcript subtitle)
         (ai/youtube--vtt-transcript subtitle))
       "captions")
    (message "No usable captions; downloading audio for local Whisper...")
    (let ((audio (ai/youtube--download-audio program url directory)))
      (cons
       (ai/youtube--whisper-transcript audio directory)
       (format "local Whisper (%s)" ai/youtube-context-whisper-model)))))

(defun ai/youtube--video-title (program url)
  "Return the title for URL using PROGRAM, or nil on failure."
  (let ((buffer (generate-new-buffer " *yt-dlp-title*")))
    (unwind-protect
        (when (eq
               (process-file
                program nil buffer nil
                "--skip-download"
                "--no-playlist"
                "--print" "%(title)s"
                url)
               0)
          (car (split-string (ai/youtube--process-output buffer) "\n" t)))
      (kill-buffer buffer))))

;;;###autoload
(defun ai/youtube-context (url)
  "Copy paste-ready transcript context for URL.

Use yt-dlp captions when available.  If the video has no usable English
captions, download its audio and transcribe it locally with Whisper.  The
prompt defaults to an HTTP URL currently in the clipboard."
  (interactive
   (list
    (read-string "Video URL: " (or (ai/youtube--clipboard-url) ""))))
  (let ((program (executable-find ai/youtube-context-program)))
    (unless program
      (user-error "%s is not available in PATH" ai/youtube-context-program))
    (unless (string-match-p "\\`https?://" url)
      (user-error "Expected an http(s) video URL"))
    (let ((directory (make-temp-file "yt-dlp-context-" t)))
      (unwind-protect
          (let* ((result (ai/youtube--transcript program url directory))
                 (transcript (car result))
                 (source (cdr result))
                 (title (or (ai/youtube--video-title program url) "Video"))
                 (context
                  (format
                   "Title: %s\nURL: %s\nTranscript source: %s\n\nTranscript:\n%s"
                   title url source transcript))
                 (words (length (split-string transcript "[[:space:]]+" t))))
            (when (string-empty-p transcript)
              (user-error "Transcript contained no text"))
            (kill-new context)
            (when (fboundp 'gui-set-selection)
              (ignore-errors (gui-set-selection 'CLIPBOARD context)))
            (message "Copied YouTube context: %d words via %s from %s"
                     words source title))
        (delete-directory directory t)))))

(provide 'youtube-context)
;;; youtube-context.el ends here
