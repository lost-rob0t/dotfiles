;;; meme.el --- Generate memes from text with OpenRouter -*- lexical-binding: t; -*-

(require 'ai)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'url-http)

(defgroup ai/meme nil
  "Generate meme images from Emacs."
  :group 'ai/llm
  :prefix "ai/meme-")

(defcustom ai/meme-directory "~/Documents/memes/AI/slop/"
  "Directory where generated meme images are stored."
  :type 'directory
  :group 'ai/meme)

(defcustom ai/meme-model "openai/gpt-image-2"
  "OpenRouter image model used to generate memes."
  :type 'string
  :group 'ai/meme)

(defcustom ai/meme-quality "high"
  "Image quality requested from the image model."
  :type '(choice (const "auto")
                 (const "low")
                 (const "medium")
                 (const "high"))
  :group 'ai/meme)

(defcustom ai/meme-size "1024x1024"
  "Requested meme image size."
  :type 'string
  :group 'ai/meme)

(defcustom ai/meme-output-format "png"
  "File format used for generated memes."
  :type '(choice (const "png")
                 (const "jpeg")
                 (const "webp"))
  :group 'ai/meme)

(defcustom ai/meme-openrouter-url "https://openrouter.ai/api/v1/images"
  "OpenRouter image-generation endpoint."
  :type 'string
  :group 'ai/meme)

(defun ai/meme--generation-prompt (source)
  "Turn SOURCE into a direct image-generation prompt for a finished meme."
  (format
   (concat
    "Create one finished internet meme from the source text below.\n\n"
    "Choose the funniest clear visual metaphor and the best meme composition "
    "without asking questions. Use bold, highly legible meme text with strong "
    "contrast and clean spacing. Preserve exact quoted wording when the source "
    "contains explicit caption text; otherwise write short punchy captions that "
    "faithfully express the source. Make the result immediately understandable, "
    "visually intense, and suitable for posting. Do not add a watermark, logo, "
    "signature, explanation, border, or UI chrome. Return only the final image.\n\n"
    "SOURCE TEXT:\n%s")
   source))

(defun ai/meme--slug (text)
  "Return a short filename-safe slug derived from TEXT."
  (let* ((slug (downcase text))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
         (slug (string-trim slug "-+" "-+")))
    (truncate-string-to-width
     (if (string-empty-p slug) "meme" slug)
     48 nil nil t)))

(defun ai/meme--output-file (source)
  "Return a unique output filename for SOURCE and create its directory."
  (let* ((directory (file-name-as-directory
                     (expand-file-name ai/meme-directory)))
         (digest (substring (secure-hash 'sha1 source) 0 8))
         (name (format "%s-%s-%s.%s"
                       (format-time-string "%Y%m%d-%H%M%S")
                       (ai/meme--slug source)
                       digest
                       ai/meme-output-format)))
    (make-directory directory t)
    (expand-file-name name directory)))

(defun ai/meme--read-json-response ()
  "Read the JSON response in the current URL retrieval buffer."
  (goto-char url-http-end-of-headers)
  (let ((json-array-type 'list)
        (json-false nil)
        (json-key-type 'symbol)
        (json-object-type 'alist))
    (json-read)))

(defun ai/meme--write-image (encoded output-file)
  "Decode base64 ENCODED image data and write it to OUTPUT-FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (base64-decode-string encoded))
    (write-region (point-min) (point-max) output-file nil 'silent)))

(defun ai/meme--insert-org-image (buffer marker output-file)
  "Insert OUTPUT-FILE at MARKER in Org BUFFER and display it inline."
  (when (and (buffer-live-p buffer)
             (marker-buffer marker))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char marker)
          (unless (bolp)
            (insert "\n"))
          (insert (format "[[file:%s]]\n" output-file))
          (org-display-inline-images))))))

(defun ai/meme--api-error-message (response)
  "Extract an API error message from RESPONSE."
  (when-let ((error-object (alist-get 'error response)))
    (or (alist-get 'message error-object)
        (format "%S" error-object))))

(defun ai/meme--handle-response (status output-file origin-buffer insertion-marker)
  "Handle OpenRouter STATUS and save the image to OUTPUT-FILE.
Insert it into ORIGIN-BUFFER at INSERTION-MARKER when that buffer is Org."
  (unwind-protect
      (condition-case err
          (progn
            (when-let ((network-error (plist-get status :error)))
              (error "Network error: %S" network-error))
            (let* ((http-status url-http-response-status)
                   (response (ai/meme--read-json-response))
                   (api-error (ai/meme--api-error-message response))
                   (image (car (alist-get 'data response)))
                   (encoded (and image (alist-get 'b64_json image))))
              (unless (and (integerp http-status)
                           (<= 200 http-status)
                           (< http-status 300))
                (error "OpenRouter returned HTTP %s: %s"
                       http-status
                       (or api-error response)))
              (when api-error
                (error "OpenRouter image generation failed: %s" api-error))
              (unless encoded
                (error "OpenRouter returned no image data"))
              (ai/meme--write-image encoded output-file)
              (ai/meme--insert-org-image
               origin-buffer insertion-marker output-file)
              (message "Meme saved: %s" output-file)))
        (error
         (message "Meme generation failed: %s"
                  (error-message-string err))))
    (set-marker insertion-marker nil)
    (kill-buffer (current-buffer))))

;;;###autoload
(defun ai/meme-generate ()
  "Generate a meme from the active region or an interactively entered prompt.
The image is saved under `ai/meme-directory'.  In Org buffers, insert a file
link at point or immediately after the selected region and display it inline."
  (interactive)
  (let* ((region-active (use-region-p))
         (source (string-trim
                  (if region-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (read-string "Meme prompt: "))))
         (origin-buffer (current-buffer))
         (insertion-marker
          (copy-marker (if region-active (region-end) (point)) t)))
    (when (string-empty-p source)
      (user-error "Meme source text cannot be empty"))
    (deactivate-mark)
    (let* ((output-file (ai/meme--output-file source))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Authorization" . ,(concat
                                    "Bearer "
                                    (ai/llm--require-api-key 'openrouter)))
              ("Content-Type" . "application/json")
              ("X-Title" . "Emacs Meme Generator")))
           (url-request-data
            (encode-coding-string
             (json-encode
              `((model . ,ai/meme-model)
                (prompt . ,(ai/meme--generation-prompt source))
                (n . 1)
                (quality . ,ai/meme-quality)
                (size . ,ai/meme-size)
                (output_format . ,ai/meme-output-format)))
             'utf-8)))
      (message "Generating meme with %s..." ai/meme-model)
      (url-retrieve
       ai/meme-openrouter-url
       (lambda (status)
         (ai/meme--handle-response
          status output-file origin-buffer insertion-marker))
       nil t t))))

(provide 'meme)
;;; meme.el ends here
