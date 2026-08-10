;;; youtube-context.el --- Copy video transcripts for LLM context -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch English subtitles with yt-dlp, normalize them to plain text, and copy
;; a compact context packet to the kill ring/system clipboard.  If captions are
;; unavailable, download the audio and transcribe it locally with OpenAI Whisper.

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
  "OpenAI Whisper executable used when a video has no usable captions."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-model "base.en"
  "Whisper model used for local English audio transcription."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-language "en"
  "Language passed to Whisper for local audio transcription."
  :type 'string
  :group 'ai/youtube-context)

(defcustom ai/youtube-context-whisper-threads 16
  "CPU threads passed to Whisper during local transcription."
  :type 'integer
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
  "Normalize subtitle or transcription TEXT into compact plain text."
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
Return the preferred subtitle file, or nil when no matching subtitles exist."
  (let ((default-directory directory)
        (buffer (generate-new-buffer " *yt-dlp-context*")))
    (unwind-protect
        (let ((status
               (process-file
                program nil buffer nil
                "--skip-download"
                "--no-playlist"
                "--write-subs"
                "--write-auto-subs"
                "--sub-langs" ai/youtube-context-sub-langs
                "--sub-format" "json3/vtt/best"
                "--output" "%(id)s.%(ext)s"
                url)))
          (unless (eq status 0)
            (error "yt-dlp subtitle fetch failed: %s"
                   (ai/youtube--process-output buffer)))
          (ai/youtube--best-subtitle directory))
      (kill-buffer buffer))))

(defun ai/youtube--subtitle-transcript (program url directory)
  "Return a non-empty caption transcript for URL, or nil."
  (when-let ((subtitle (ai/youtube--download-subtitle program url directory)))
    (let ((transcript
           (if (string-suffix-p ".json3" subtitle)
               (ai/youtube--json3-transcript subtitle)
             (ai/youtube--vtt-transcript subtitle))))
      (unless (string-empty-p transcript)
        transcript))))

(defun ai/youtube--download-audio (program url directory)
  "Use yt-dlp PROGRAM to download best audio for URL into DIRECTORY."
  (let ((default-directory directory)
        (buffer (generate-new-buffer " *yt-dlp-audio*")))
    (unwind-protect
        (let ((status
               (process-file
                program nil buffer nil
                "--no-playlist"
                "--format" "bestaudio/best"
                "--output" "audio.%(ext)s"
                url)))
          (unless (eq status 0)
            (error "yt-dlp audio download failed: %s"
                   (ai/youtube--process-output buffer)))
          (or (car (directory-files directory t "\\`audio\\.[^.]+\\'"))
              (error "yt-dlp completed without producing an audio file")))
      (kill-buffer buffer))))

(defun ai/youtube--read-text-file (file)
  "Return normalized text contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (ai/youtube--normalize-text (buffer-string))))

(defun ai/youtube--whisper-transcript (yt-dlp-program url directory)
  "Download URL audio with YT-DLP-PROGRAM and transcribe it in DIRECTORY."
  (let ((whisper (executable-find ai/youtube-context-whisper-program)))
    (unless whisper
      (user-error
       "No usable captions and %s is not available in PATH"
       ai/youtube-context-whisper-program))
    (unless (executable-find "ffmpeg")
      (user-error "No usable captions and ffmpeg is required by Whisper"))
    (let* ((audio (ai/youtube--download-audio yt-dlp-program url directory))
           (output (expand-file-name
                    (concat (file-name-base audio) ".txt")
                    directory))
           (buffer (generate-new-buffer " *whisper-context*")))
      (message "No usable captions; transcribing audio locally with Whisper %s..."
               ai/youtube-context-whisper-model)
      (unwind-protect
          (let ((status
                 (process-file
                  whisper nil buffer nil
                  audio
                  "--model" ai/youtube-context-whisper-model
                  "--device" "cpu"
                  "--language" ai/youtube-context-whisper-language
                  "--task" "transcribe"
                  "--output_format" "txt"
                  "--output_dir" directory
                  "--verbose" "False"
                  "--fp16" "False"
                  "--threads" (number-to-string ai/youtube-context-whisper-threads))))
            (unless (eq status 0)
              (error "Whisper transcription failed: %s"
                     (ai/youtube--process-output buffer)))
            (unless (file-exists-p output)
              (error "Whisper completed without producing %s" output))
            (let ((transcript (ai/youtube--read-text-file output)))
              (when (string-empty-p transcript)
                (error "Whisper produced an empty transcript"))
              transcript))
        (kill-buffer buffer)))))

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
  "Copy a transcript context packet for video URL.

English human or automatic captions are preferred.  If no usable captions are
available, download the audio with yt-dlp and transcribe it locally with
OpenAI Whisper.  The prompt defaults to an HTTP URL currently in the clipboard.
The copied packet contains the video title, URL, transcript source, and text."
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
          (let* ((caption-transcript
                  (ai/youtube--subtitle-transcript program url directory))
                 (source
                  (if caption-transcript
                      "video captions"
                    (format "local Whisper (%s)"
                            ai/youtube-context-whisper-model)))
                 (transcript
                  (or caption-transcript
                      (ai/youtube--whisper-transcript program url directory)))
                 (title (or (ai/youtube--video-title program url) "Video"))
                 (context
                  (format
                   "Title: %s\nURL: %s\nTranscript source: %s\n\nTranscript:\n%s"
                   title url source transcript))
                 (words (length (split-string transcript "[[:space:]]+" t))))
            (kill-new context)
            (when (fboundp 'gui-set-selection)
              (ignore-errors (gui-set-selection 'CLIPBOARD context)))
            (message "Copied YouTube context: %d words from %s via %s"
                     words title source))
        (delete-directory directory t)))))

(provide 'youtube-context)
;;; youtube-context.el ends here
