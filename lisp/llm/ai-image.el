;;; ai-image.el --- OpenAI image-generation tools for Emacs -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-prompts)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defgroup ai/image nil
  "OpenAI image generation and editing."
  :group 'applications
  :prefix "ai/image-")

(defcustom ai/image-responses-endpoint "https://api.openai.com/v1/responses"
  "OpenAI Responses API endpoint used for image-tool calls."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-responses-model "gpt-5.2"
  "OpenAI model that invokes the hosted image-generation tool."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-model "gpt-image-1.5"
  "Image model passed to the OpenAI image-generation tool."
  :type 'string
  :group 'ai/image)

(defcustom ai/image-size "1024x1024"
  "Default generated image size."
  :type '(choice (const "1024x1024")
                 (const "1024x1536")
                 (const "1536x1024")
                 (const "auto"))
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
  "When non-nil, open a generated image after saving it."
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

(defun ai/image--tool (&optional action)
  "Return the image-generation tool declaration for ACTION."
  (append
   `((type . "image_generation")
     (model . ,ai/image-model)
     (size . ,ai/image-size)
     (quality . ,ai/image-quality)
     (background . "auto")
     (output_format . ,ai/image-output-format))
   (when action `((action . ,action)))
   (when (equal action "edit") '((input_fidelity . "high")))))

(defun ai/image--request-body (input &optional action)
  "Build a Responses API body from INPUT and optional image ACTION."
  `((model . ,ai/image-responses-model)
    (input . ,input)
    (tools . ,(vector (ai/image--tool action)))
    (tool_choice . ((type . "image_generation")))))

(defun ai/image--read-json-response ()
  "Read the JSON body from the current URL response buffer."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "Malformed HTTP response"))
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read)))

(defun ai/image--api-error (payload)
  "Return a useful API error string from PAYLOAD."
  (or (alist-get 'message (alist-get 'error payload))
      (alist-get 'error payload)
      "OpenAI image request failed"))

(defun ai/image--result (payload)
  "Return the base64 image result from Responses API PAYLOAD."
  (when-let ((call
              (cl-find-if
               (lambda (item)
                 (equal (alist-get 'type item) "image_generation_call"))
               (alist-get 'output payload))))
    (alist-get 'result call)))

(defun ai/image--write-result (encoded file)
  "Decode base64 ENCODED image data and write it to FILE."
  (let ((data (base64-decode-string encoded)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) file nil 'silent))))
  file)

(defun ai/image--response-callback (status output-file callback)
  "Handle image HTTP STATUS, writing OUTPUT-FILE and invoking CALLBACK."
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
                (error "OpenAI response contained no image_generation_call result"))
              (ai/image--write-result result output-file)
              (when ai/image-open-after-generate
                (find-file-other-window output-file))
              (when callback
                (funcall callback output-file))
              (message "Image saved: %s" output-file)))
        (error
         (message "Image generation failed: %s" (error-message-string err))))
    (kill-buffer (current-buffer))))

(defun ai/image--request (input output-file &optional action callback)
  "Send image INPUT and asynchronously write OUTPUT-FILE.
ACTION is nil for generation or \="edit\=" for image editing.  CALLBACK is
called with OUTPUT-FILE after a successful request."
  (let* ((key (ai/llm--require-api-key 'openai))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))
            ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode (ai/image--request-body input action)) 'utf-8)))
    (url-retrieve ai/image-responses-endpoint
                  #'ai/image--response-callback
                  (list output-file callback)
                  t t)
    (message "Generating image with %s / %s..."
             ai/image-responses-model ai/image-model)))

(defun ai/image-generate (prompt &optional output-file)
  "Generate an image from PROMPT using OpenAI's image-generation tool."
  (interactive (list (ai/image--prompt)))
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (user-error "Image prompt is empty"))
  (ai/image--request prompt (or output-file (ai/image--filename))))

(defun ai/image-generate-template (&optional name)
  "Fill reusable prompt template NAME and generate an image from it."
  (interactive)
  (ai/image-generate (ai/prompt-template-render name)))

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

(defun ai/image-edit (file prompt &optional output-file)
  "Edit image FILE according to PROMPT using OpenAI's image-generation tool."
  (interactive
   (list (read-file-name "Image to edit: " nil nil t)
         (ai/image--prompt)))
  (unless (and prompt (not (string-empty-p (string-trim prompt))))
    (user-error "Image edit prompt is empty"))
  (let ((input
         (vector
          `((role . "user")
            (content . ,(vector
                         `((type . "input_text") (text . ,prompt))
                         `((type . "input_image")
                           (image_url . ,(ai/image--data-url file)))))))))
    (ai/image--request input (or output-file (ai/image--filename "edit"))
                       "edit")))

(provide 'ai-image)
;;; ai-image.el ends here
