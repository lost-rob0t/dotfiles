;;; flashcards.el --- Org-drill flashcards via gptel -*- lexical-binding: t; -*-

(require 'org)
(require 'gptel)

(let ((ai-lib (expand-file-name "ai.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p ai-lib)
    (load ai-lib nil t)))

(defgroup flashcards nil
  "Generate org-drill flashcards with gptel."
  :group 'org
  :group 'gptel)

(defcustom flashcards-preset 'org-flashcards-opus
  "gptel preset to use for flashcard generation.
This should be a symbol created with `gptel-make-preset'."
  :type 'symbol)

;; You can adjust backend/model/etc or replace this preset entirely.
;; Model is a symbol on purpose; fix it to whatever Opus you actually use.
(gptel-make-preset 'org-flashcards-opus
  :description "Org-drill flashcard generator using Opus"
  :backend (ai/llm-openrouter-backend :stream t :name "OpenRouter")
  :model (ai/llm-resolve-model)
  :temperature 0.3
  :system
  "You convert notes into high-quality Org-mode flashcards for org-drill.
Requirements:

- Output ONLY Org-mode headings and text, no prose or explanation.
- Assume your output will be inserted UNDER an existing \"* Flashcards\" heading.
- For each flashcard, create a level-2 heading like:
  ** <short title> :drill:flascard:question:
- Put the QUESTION text in the body of that heading (plain text or cloze).
- Put the ANSWER in a level-3 child heading:
  *** Answer :awsner:
  <answer here>
- Use org-drill conventions:
  - Simple cards: question text in the body, answer under the child heading.
  - Cloze deletions: [like this] in the body, or multicloze with DRILL_CARD_TYPE.
- Prefer small, atomic facts per card.
- Use Org links (e.g. [[https://example.com][desc]], [[*Other heading]]) where relevant.
- Use an Org tree structure only, no JSON, no markdown.

Return multiple cards if appropriate, but keep them concise and unambiguous.")

(defun flashcards--region-or-subtree ()
  "Return text for the active region, or the current Org subtree."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (org-with-wide-buffer
     (org-back-to-heading t)
     (let ((beg (point)))
       (org-end-of-subtree t t)
       (buffer-substring-no-properties beg (point))))))

(defun flashcards--ensure-heading ()
  "Ensure a top-level \"* Flashcards\" heading exists.
Return a marker at the end of its subtree, where new cards should be inserted."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let (pos)
     (if (re-search-forward "^\\* +Flashcards\\b" nil t)
         (setq pos (point))
       ;; Create it at end of buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "* Flashcards :ai:flashcards:\n")
       (setq pos (point)))
     (goto-char pos)
     (org-end-of-subtree t t)
     (unless (bolp) (insert "\n"))
     (point-marker))))

(defun flashcards--prompt (source)
  "Build the LLM prompt string from SOURCE notes."
  (format
   (concat
    "You are generating org-drill flashcards from the following Org notes.\n"
    "Follow the flashcard format described in your system instructions.\n"
    "Use small, atomic facts. Prefer cloze deletions where natural.\n"
    "SOURCE NOTES:\n\n%s\n")
   source))

;;;###autoload
(defun org-gptel-generate-flashcards (beg end)
  "Generate org-drill flashcards from region or subtree using gptel.

If region is active, use it as the source. Otherwise, use the
current Org subtree. The resulting cards are inserted under the
\"* Flashcards\" heading in the current buffer."
  (interactive "r")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((source (flashcards--region-or-subtree))
         (insert-marker (flashcards--ensure-heading))
         (prompt (flashcards--prompt source)))
    (gptel-with-preset flashcards-preset
      (gptel-request
          prompt
        :callback
        (lambda (response info)
          (ignore info)
          (when (stringp response)
            (when (marker-buffer insert-marker)
              (with-current-buffer (marker-buffer insert-marker)
                (save-excursion
                  (goto-char insert-marker)
                  ;; Make sure there's a clean separation
                  (unless (bolp) (insert "\n"))
                  (insert response)
                  (unless (bolp) (insert "\n")))))))))))


(provide 'flashcards)
;;; flashcards.el ends here
