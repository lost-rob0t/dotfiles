;;; ai-image.el --- OpenRouter image-generation tools for Emacs -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-prompts)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defgroup ai/image nil
  "OpenRouter image generation and editing."
  :group 'applications
  :prefix "ai/image-")

(defcustom ai/image-endpoint "https://openrouter.ai/api/v1/images"
  "OpenRouter dedicated image-generation endpoint."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-model "openai/gpt-image-2"
  "OpenRouter image model used for generation and editing."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-size "1024x1024"
  "Default generated image size."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-quality "high"
  "Default generated image quality."
  :type '(choice (const "low")
                 (const "medium")
                 (const "high")
                 (const "auto"))
  :group 'ai/image)

(defcustom ai/image-output-format "png"
  "Default generated image format."
  :type '(choice (const "png") (const "webp") (const "jpeg"))
  :group 'ai/image)

(defcustom ai/image-output-directory
  (expand-file-name "ai/" (or (getenv "XDG_PICTURES_DIR") "~/Pictures/"))
  "Directory where generated images are written."
  :type 'directory
  :group 'ai/image)

(defcustom ai/image-open-after-generate t
  "When non-nil, open interactively generated images outside Org buffers."
  :type 'boolean
  :group 'ai/image)

(defun ai/image--prompt ()
  "Return an image prompt from the active region or minibuffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "Image prompt: ")))

(defun ai/image--filename (&optional stem)
  "Return a fresh output filename using optional STEM."
  (make-directory ai/image-output-directory t)
  (expand-file-name
   (format "%s-%s.%s"
           (or stem "image")
           (format-time-string "%Y%m%d-%H%M%S-%3N")
           ai/image-output-format)
   ai/image-output-directory))

(defun ai/image--mime-type (file)
  "Return a supported image MIME type for FILE."
  (pcase (downcase (or (file-name-extension file) ""))
    ("png" "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("webp" "image/webp")
    (_ (user-error "Unsupported image format: %s" file))))

(defun ai/image--data-url (file)
  "Return FILE encoded as an image data URL."
  (unless (file-readable-p file)
    (user-error "Image is not readable: %s" file))
  (let ((mime (ai/image--mime-type file)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (format "data:%s;base64,%s"
              mime (base64-encode-string (buffer-string) t)))))

(defun ai/image--request-body (prompt &optional reference-file)
  "Build an OpenRouter image request for PROMPT and optional REFERENCE-FILE."
  (append
   `((model . ,ai/image-model)
     (prompt . ,prompt)
     (n . 1)
     (size . ,ai/image-size)
     (quality . ,ai/image-quality)
     (output_format . ,ai/image-output-format)
     (background . "auto"))
   (when reference-file
     `((input_references
        . ,(vector
            `((type . "image_url")
              (image_url . ((url . ,(ai/image--data-url reference-file)))))))))))

(defun ai/image--read-json-response ()
  "Read JSON from the current URL response buffer."
  (goto-char (or (and (boundp 'url-http-end-of-headers)
                      url-http-end-of-headers)
                 (point-min)))
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read)))

(defun ai/image--api-error (payload)
  "Return a useful OpenRouter API error string from PAYLOAD."
  (let ((error-object (alist-get 'error payload)))
    (cond
     ((and (listp error-object) (alist-get 'message error-object))
      (alist-get 'message error-object))
     (error-object (format "%S" error-object))
     (t "OpenRouter image request failed"))))

(defun ai/image--result (payload)
  "Return base64 image data from OpenRouter PAYLOAD."
  (when-let ((image (car (alist-get 'data payload))))
    (alist-get 'b64_json image)))

(defun ai/image--cost (payload)
  "Return reported USD cost from OpenRouter PAYLOAD, or nil."
  (alist-get 'cost (alist-get 'usage payload)))

(defun ai/image--write-result (encoded file)
  "Decode base64 ENCODED image data and write it to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (base64-decode-string encoded))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent)))
  file)

(defun ai/image--org-link (file)
  "Return an Org file link for FILE."
  (format "[[file:%s]]" (expand-file-name file)))

(defun ai/image--tool-result (file payload)
  "Return a gptel tool result for generated FILE and PAYLOAD."
  (let ((cost (ai/image--cost payload)))
    (concat
     (format "Generated image with %s" ai/image-model)
     (if (numberp cost) (format " (USD %.4f)" cost) "")
     ". Include the exact Org link below in your final response so it renders inline.\n"
     (ai/image--org-link file))))

(defun ai/image--insert-org-image (buffer marker file)
  "Insert FILE at MARKER in Org BUFFER and display it inline."
  (when (and (buffer-live-p buffer)
             marker
             (marker-buffer marker))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char marker)
          (unless (bolp)
            (insert "\n"))
          (insert (ai/image--org-link file) "\n")
          (org-display-inline-images))))))

(defun ai/image--response-callback
    (status output-file callback origin-buffer insertion-marker open-after)
  "Handle OpenRouter STATUS and write OUTPUT-FILE.
CALLBACK receives the tool result string.  ORIGIN-BUFFER and INSERTION-MARKER
are used by interactive Org commands.  OPEN-AFTER opens non-Org results."
  (unwind-protect
      (condition-case err
          (let* ((http-status (and (boundp 'url-http-response-status)
                                   url-http-response-status))
                 (payload (ai/image--read-json-response)))
            (when (or (plist-get status :error)
                      (and http-status (>= http-status 400)))
              (error "%s" (ai/image--api-error payload)))
            (let ((result (ai/image--result payload)))
              (unless result
                (error "OpenRouter returned no image data"))
              (ai/image--write-result result output-file)
              (ai/image--insert-org-image origin-buffer insertion-marker output-file)
              (when (and open-after
                         (not (and (buffer-live-p origin-buffer)
                                   (with-current-buffer origin-buffer
                                     (derived-mode-p 'org-mode)))))
                (find-file-other-window output-file))
              (when callback
                (funcall callback (ai/image--tool-result output-file payload)))
              (message "Image saved: %s" output-file)))
        (error
         (let ((message-text
                (format "Image generation failed: %s" (error-message-string err))))
           (when callback
             (funcall callback (concat "ERROR: " message-text)))
           (message "%s" message-text))))
    (when insertion-marker
      (set-marker insertion-marker nil))
    (kill-buffer (current-buffer))))

(defun ai/image--request
    (prompt output-file &optional reference-file callback origin-buffer insertion-marker open-after)
  "Generate an image for PROMPT and asynchronously write OUTPUT-FILE.
REFERENCE-FILE performs image-to-image generation.  CALLBACK is used by async
gptel tools.  ORIGIN-BUFFER and INSERTION-MARKER support inline Org display."
  (let* ((key (ai/llm--require-api-key 'openrouter))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))
            ("Content-Type" . "application/json")
            ("X-Title" . "Emacs gptel image tools")))
         (url-request-data
          (encode-coding-string
           (json-encode (ai/image--request-body prompt reference-file)) 'utf-8)))
    (url-retrieve
     ai/image-endpoint
     #'ai/image--response-callback
     (list output-file callback origin-buffer insertion-marker open-after)
     t t)
    (message "Generating image with %s..." ai/image-model)))

(defun ai/image--interactive-target ()
  "Return origin-buffer and insertion marker for an interactive request."
  (let ((buffer (current-buffer)))
    (list buffer
          (when (derived-mode-p 'org-mode)
            (copy-marker (if (use-region-p) (region-end) (point)) t)))))

(defun ai/image-generate (prompt &optional output-file)
  "Generate an image from PROMPT using OpenRouter."
  (interactive (list (ai/image--prompt)))
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (user-error "Image prompt is empty"))
  (pcase-let ((`(,origin-buffer ,marker) (ai/image--interactive-target)))
    (deactivate-mark)
    (ai/image--request
     prompt
     (or output-file (ai/image--filename))
     nil nil origin-buffer marker ai/image-open-after-generate)))

(defun ai/image-generate-template (&optional name)
  "Fill reusable prompt template NAME and generate an image from it."
  (interactive)
  (ai/image-generate (ai/prompt-template-render name)))

(defun ai/image-edit (file prompt &optional output-file)
  "Edit image FILE according to PROMPT using OpenRouter."
  (interactive
   (list (read-file-name "Image to edit: " nil nil t)
         (ai/image--prompt)))
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (user-error "Image edit prompt is empty"))
  (pcase-let ((`(,origin-buffer ,marker) (ai/image--interactive-target)))
    (deactivate-mark)
    (ai/image--request
     prompt
     (or output-file (ai/image--filename "edit"))
     file nil origin-buffer marker ai/image-open-after-generate)))

(defun ai/image-tool-generate (prompt callback)
  "Async gptel tool: generate an image for PROMPT and invoke CALLBACK."
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (funcall callback "ERROR: Image prompt is empty"))
  (ai/image--request
   prompt (ai/image--filename "chat-image") nil callback nil nil nil))

(defun ai/image-tool-edit (file prompt callback)
  "Async gptel tool: edit FILE according to PROMPT and invoke CALLBACK."
  (if (not (file-readable-p file))
      (funcall callback (format "ERROR: Image is not readable: %s" file))
    (ai/image--request
     prompt (ai/image--filename "chat-edit") file callback nil nil nil)))

(defun ai/image-tool-list-templates ()
  "Return available reusable prompt-template names."
  (let ((names (ai/prompt-template-names)))
    (if names
        (mapconcat #'identity names "\n")
      "No reusable prompt templates are installed.")))

(defun ai/image-tool-read-template (name)
  "Return raw reusable prompt template NAME."
  (ai/prompt-template--read name))

(provide 'ai-image)
;;; ai-image.el ends here
