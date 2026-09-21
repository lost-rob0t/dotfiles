;;; org-food.el --- Org-Roam recipes and inventory -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'kb-roam)

(defgroup org-food nil
  "Org-Roam recipes, food inventory, and symbolic Mega Brain hooks."
  :group 'org
  :prefix "org-food-")

(defcustom org-food-default-visibility "private"
  "Visibility used for newly-created recipes."
  :type '(choice (const "private") (const "unlisted") (const "public"))
  :group 'org-food)

(defconst org-food-event-deltas
  '(("receive" . 1)
    ("buy" . 1)
    ("return" . 1)
    ("consume" . -1)
    ("waste" . -1)
    ("adjust_add" . 1)
    ("adjust_remove" . -1)
    ("ordered" . 0)
    ("putaway" . 0)
    ("move" . 0)
    ("open" . 0)))

(defun org-food--keyword (name)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote name))
             nil t)
        (string-trim (match-string-no-properties 1))))))

(defun org-food--files ()
  (directory-files-recursively (kb-roam-root) "\\.org\\'"))

(defun org-food--topical-tags (cuisine)
  (delete-dups
   (list "recipe"
         (if (string-empty-p (string-trim cuisine))
             "food"
           (kb-roam--normalize-tag cuisine)))))

(defun org-food-create-recipe (title cuisine servings ingredients instructions)
  "Create a human-authored Org-Roam recipe."
  (interactive
   (list (read-string "Recipe title: ")
         (read-string "Cuisine: ")
         (read-string "Servings: " "2")
         (read-string "Ingredients (use ; between items): ")
         (read-string "Instructions: ")))
  (let* ((ingredient-lines
          (mapconcat
           (lambda (item) (concat "- " (string-trim item)))
           (split-string ingredients ";" t)
           "\n"))
         (body
          (format
           "#+CUISINE: %s\n#+SERVINGS: %s\n\n* Ingredients\n%s\n\n* Instructions\n%s\n"
           cuisine servings ingredient-lines instructions))
         (path
          (kb-roam-write-note
           title body (org-food--topical-tags cuisine) "recipe"
           "emacs:org-food" org-food-default-visibility nil)))
    (find-file path)
    path))

(defun org-food--generation-prompt (title cuisine servings constraints)
  (format
   (concat
    "Write an Org-mode recipe body only. Do not emit file metadata or a title. "
    "Use exactly these top-level headings: * Ingredients, * Instructions, * Notes. "
    "Ingredients must be list items with practical quantities and units. "
    "Recipe title: %s. Cuisine: %s. Servings: %s. Constraints/preferences: %s.")
   title cuisine servings
   (if (string-empty-p constraints) "none" constraints)))

(defun org-food-generate-recipe (title cuisine servings constraints)
  "Generate a recipe through the configured gptel backend and tag it as AI."
  (interactive
   (list (read-string "Recipe title/idea: ")
         (read-string "Cuisine: ")
         (read-string "Servings: " "2")
         (read-string "Constraints/preferences: ")))
  (require 'gptel)
  (when (require 'ai nil t)
    (ai/llm-apply-defaults))
  (let ((prompt (org-food--generation-prompt
                 title cuisine servings constraints)))
    (message "Generating recipe with %s / %s..."
             (if (boundp 'ai/llm-provider) ai/llm-provider 'gptel)
             (if (boundp 'ai/llm-model) ai/llm-model gptel-model))
    (gptel-request
     prompt
     :callback
     (lambda (response info)
       (if (not (stringp response))
           (message "Recipe generation failed: %s"
                    (or (plist-get info :status) "unknown error"))
         (let* ((body
                 (format "#+CUISINE: %s\n#+SERVINGS: %s\n\n%s"
                         cuisine servings (string-trim response)))
                (path
                 (kb-roam-write-note
                  title body (org-food--topical-tags cuisine) "recipe"
                  "gptel:starintel-27b" org-food-default-visibility t)))
           (message "AI recipe saved: %s" path)
           (find-file path)))))))

(defun org-food--recipe-record (file)
  (with-temp-buffer
    (insert-file-contents file nil 0 65536)
    (org-mode)
    (when (string= (org-food--keyword "ROAM_KIND") "recipe")
      (list :file file
            :title (or (org-food--keyword "TITLE") (file-name-base file))
            :cuisine (or (org-food--keyword "CUISINE") "")
            :servings (or (org-food--keyword "SERVINGS") "")
            :source (or (org-food--keyword "SOURCE") "")
            :ai (if (member
                     (downcase (or (org-food--keyword "AI_GENERATED") ""))
                     '("t" "true" "yes" "1"))
                    "yes"
                  "no")))))

(defun org-food--recipe-records ()
  (delq nil (mapcar #'org-food--recipe-record (org-food--files))))

(defun org-food-find-recipe ()
  "Open an Org-Roam recipe selected by title."
  (interactive)
  (let* ((rows (org-food--recipe-records))
         (choices
          (mapcar
           (lambda (row)
             (cons (plist-get row :title) (plist-get row :file)))
           rows)))
    (unless choices
      (user-error "No recipes found"))
    (find-file
     (cdr
      (assoc
       (completing-read "Recipe: " choices nil t)
       choices)))))

(defun org-food--inventory-events ()
  (let (events)
    (dolist (file (org-food--files))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (when (string= (org-entry-get nil "KIND") "inventory-event")
             (let* ((event (or (org-entry-get nil "EVENT") ""))
                    (adjustment (or (org-entry-get nil "ADJUSTMENT") ""))
                    (normalized
                     (if (string= event "adjust")
                         (concat "adjust_" adjustment)
                       event))
                    (qty (string-to-number
                          (or (org-entry-get nil "QTY") "0"))))
               (push
                (list :item-key (or (org-entry-get nil "ITEM_KEY") "")
                      :event normalized
                      :qty qty
                      :unit (or (org-entry-get nil "UNIT") ""))
                events))))
         nil 'file)))
    (nreverse events)))

(defun org-food--stock-table ()
  (let ((totals (make-hash-table :test #'equal))
        (units (make-hash-table :test #'equal)))
    (dolist (event (org-food--inventory-events))
      (let* ((key (plist-get event :item-key))
             (kind (plist-get event :event))
             (qty (plist-get event :qty))
             (factor (cdr (assoc kind org-food-event-deltas))))
        (when (and factor (not (string-empty-p key)))
          (puthash key
                   (+ (gethash key totals 0.0) (* factor qty))
                   totals)
          (puthash key (plist-get event :unit) units))))
    (list totals units)))

(defun org-food--inventory-records ()
  (let* ((stock (org-food--stock-table))
         (totals (nth 0 stock))
         (units (nth 1 stock))
         rows)
    (dolist (file (org-food--files))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (when (string= (org-entry-get nil "KIND") "inventory-item")
             (let* ((key (or (org-entry-get nil "ITEM_KEY") ""))
                    (name (or (org-entry-get nil "NAME")
                              (org-get-heading t t t t)))
                    (unit (or (org-entry-get nil "UNIT")
                              (gethash key units "")
                              ""))
                    (quantity (gethash key totals 0.0)))
               (push
                (list :id (or (org-entry-get nil "ID") key)
                      :file file
                      :name name
                      :category (or (org-entry-get nil "CATEGORY") "")
                      :quantity quantity
                      :unit unit
                      :location (or (org-entry-get nil "DEFAULT_LOCATION") "")
                      :reorder-at (or (org-entry-get nil "REORDER_AT") ""))
                rows))))
         nil 'file)))
    (nreverse rows)))

(defun org-food--format-number (value)
  (if (= value (truncate value))
      (number-to-string (truncate value))
    (format "%.2f" value)))

(define-derived-mode org-food-recipe-table-mode tabulated-list-mode "Recipes"
  "Table view of every Org-Roam recipe."
  (setq tabulated-list-format
        [("Recipe" 34 t)
         ("Cuisine" 18 t)
         ("Servings" 10 t)
         ("AI" 5 t)
         ("Source" 32 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun org-food-recipe-table ()
  "Show all recipes in a sortable table."
  (interactive)
  (let ((buffer (get-buffer-create "*Recipes*")))
    (with-current-buffer buffer
      (org-food-recipe-table-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (row)
               (list
                (plist-get row :file)
                (vector
                 (plist-get row :title)
                 (plist-get row :cuisine)
                 (plist-get row :servings)
                 (plist-get row :ai)
                 (plist-get row :source))))
             (org-food--recipe-records)))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(define-derived-mode org-food-inventory-table-mode tabulated-list-mode "Foods"
  "Table view of current food inventory."
  (setq tabulated-list-format
        [("Food" 30 t)
         ("Category" 16 t)
         ("Qty" 10 t)
         ("Unit" 10 t)
         ("Location" 18 t)
         ("Reorder" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun org-food-inventory-table ()
  "Show current inventory derived from immutable Org daily events."
  (interactive)
  (let ((buffer (get-buffer-create "*Food Inventory*")))
    (with-current-buffer buffer
      (org-food-inventory-table-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (row)
               (list
                (plist-get row :id)
                (vector
                 (plist-get row :name)
                 (plist-get row :category)
                 (org-food--format-number (plist-get row :quantity))
                 (plist-get row :unit)
                 (plist-get row :location)
                 (plist-get row :reorder-at))))
             (org-food--inventory-records)))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun org-food-record-event (event item-key qty unit &optional adjustment)
  "Append an immutable inventory EVENT to today's Org daily file."
  (interactive
   (list
    (completing-read
     "Event: "
     '("ordered" "receive" "buy" "putaway" "move" "open"
       "consume" "waste" "return" "adjust")
     nil t)
    (read-string "Item key: ")
    (read-number "Quantity: " 1)
    (read-string "Unit: " "each")
    nil))
  (when (or (not (numberp qty)) (<= qty 0))
    (user-error "Quantity must be positive"))
  (let* ((adjustment
          (if (string= event "adjust")
              (or adjustment
                  (completing-read "Adjustment: " '("add" "remove") nil t))
            ""))
         (day (format-time-string "%Y-%m-%d"))
         (dir (expand-file-name "daily" (kb-roam-root)))
         (file (expand-file-name (concat day ".org") dir))
         (event-id (org-id-new)))
    (make-directory dir t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+TITLE: " day "\n"
                "#+ROAM_SCHEMA: org-roam-meta/v1\n"
                "#+ROAM_KIND: daily\n"
                "#+ROAM_VISIBILITY: private\n"
                "#+CENSOR_PROFILE: strict\n"
                "#+FILETAGS: :daily:inventory:\n\n")))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert
       (format
        (concat "\n* INVENTORY %s %s\n"
                ":PROPERTIES:\n"
                ":ID: %s\n:KIND: inventory-event\n:EVENT: %s\n"
                ":ITEM_KEY: %s\n:QTY: %s\n:UNIT: %s\n"
                ":ADJUSTMENT: %s\n:SOURCE: emacs:org-food\n"
                ":AT: %s\n:END:\n")
        event item-key event-id event item-key qty unit adjustment
        (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (save-buffer))
    (when (fboundp 'org-roam-db-sync)
      (org-roam-db-sync))
    event-id))

(defun org-food-mega-brain-status ()
  "Display the persistent Prolog Mega Brain status."
  (interactive)
  (require 'mega-brain)
  (let ((status (star/mega-brain-status)))
    (message "Mega Brain: %s predicates"
             (length (gethash "predicates" status)))))

(defun org-food-mega-brain-reset ()
  "Reload Org-derived facts and expert rules."
  (interactive)
  (require 'mega-brain)
  (star/mega-brain-reset))

(transient-define-prefix org-food-menu ()
  "Recipe, food inventory, Org-Roam, and Mega Brain commands."
  [["Recipes"
    ("n" "New recipe" org-food-create-recipe)
    ("g" "Generate with AI" org-food-generate-recipe)
    ("f" "Find recipe" org-food-find-recipe)
    ("r" "Recipe table" org-food-recipe-table)]
   ["Inventory"
    ("i" "Food table" org-food-inventory-table)
    ("e" "Record event" org-food-record-event)]
   ["Mega Brain"
    ("s" "Status" org-food-mega-brain-status)
    ("R" "Reload facts" org-food-mega-brain-reset)
    ("U" "Unwind Prolog" star/mega-brain-unwind-all)]])

(defun org-food-register-zara-bridge ()
  "Expose closed food UI operations through the Zara Emacs bridge."
  (when (require 'zara nil t)
    (zara-bridge-register-handler
     "food.recipes.table"
     (lambda (_args)
       (org-food-recipe-table)
       '((opened . "recipes"))))
    (zara-bridge-register-handler
     "food.inventory.table"
     (lambda (_args)
       (org-food-inventory-table)
       '((opened . "inventory"))))))

(with-eval-after-load 'zara
  (org-food-register-zara-bridge))

(provide 'org-food)
;;; org-food.el ends here
