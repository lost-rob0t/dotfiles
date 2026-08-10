;;; ai-image-tools.el --- gptel tools for inline image generation -*- lexical-binding: t; -*-

(require 'ai-image)
(require 'cl-lib)
(require 'gptel)

(defconst ai/image-agent-instructions
  "

Image and prompt-template rules:
- When the user asks to create, draw, render, generate, redesign, or edit an image, use GenerateImage or EditImage instead of only describing what the image should look like.
- Use ListPromptTemplates and ReadPromptTemplate when the user refers to a reusable visual prompt or named design system. Preserve the template's supplied wording and constraints when building the final image prompt.
- GenerateImage and EditImage return an Org file link. Include that exact [[file:...]] link in the final response. Do not replace it with a pathname or Markdown link. The chat UI renders the Org link inline.
- Do not claim an image was generated until the image tool returns successfully.")

(defun ai/image--clear-gptel-tool (name)
  "Remove an existing gptel tool named NAME before re-registering it."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool name) nil))))

(defun ai/image-register-gptel-tools ()
  "Register image generation, editing, and prompt-template tools with gptel."
  (dolist (name '("GenerateImage" "EditImage"
                  "ListPromptTemplates" "ReadPromptTemplate"))
    (ai/image--clear-gptel-tool name))

  (gptel-make-tool
   :name "GenerateImage"
   :function #'ai/image-tool-generate
   :category "image"
   :description
   "Generate a finished image from a complete prompt with GPT Image 2 through OpenRouter. Returns an Org file link that must be included verbatim in the final response."
   :args '((:name "prompt"
            :type string
            :description "Complete image-generation prompt"))
   :async t
   :include t)

  (gptel-make-tool
   :name "EditImage"
   :function #'ai/image-tool-edit
   :category "image"
   :description
   "Edit a local PNG, JPEG, or WebP from a complete edit instruction using GPT Image 2 through OpenRouter. Returns an Org file link that must be included verbatim in the final response."
   :args '((:name "file"
            :type string
            :description "Absolute or expanded local image path")
           (:name "prompt"
            :type string
            :description "Complete image-edit instruction"))
   :async t
   :include t)

  (gptel-make-tool
   :name "ListPromptTemplates"
   :function #'ai/image-tool-list-templates
   :category "image"
   :description "List reusable .prompt template names available to the Emacs image workflow."
   :include t)

  (gptel-make-tool
   :name "ReadPromptTemplate"
   :function #'ai/image-tool-read-template
   :category "image"
   :description "Read one reusable .prompt template verbatim before composing an image prompt."
   :args '((:name "name"
            :type string
            :description "Template name without the .prompt extension"))
   :include t)

  (when (boundp 'ai/agent-tools)
    (dolist (name '("GenerateImage" "EditImage"
                    "ListPromptTemplates" "ReadPromptTemplate"))
      (cl-pushnew name ai/agent-tools :test #'equal)))
  ai/agent-tools)

(provide 'ai-image-tools)
;;; ai-image-tools.el ends here
