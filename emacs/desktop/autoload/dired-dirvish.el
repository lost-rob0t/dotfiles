;;; dired-dirvish.el --- Fast Dirvish-backed Dired defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep ordinary directory visits cheap and predictable while retaining the
;; richer Dirvish UI on demand.  In particular, do not eagerly compute Git/VC
;; metadata or start preview processes for every Dired buffer.  Large local
;; directories use Dirvish's asynchronous fd path when fd is available.

;;; Code:

;;;###autoload
(with-eval-after-load 'dired
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group" dired-dwim-target t dired-recursive-copies 'always dired-recursive-deletes 'top delete-by-moving-to-trash t)
  (when
      (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t))
  (when
      (boundp 'dired-mouse-drag-files)
    (setq dired-mouse-drag-files t)))


;;;###autoload
(progn
  (setq dirvish-attributes
	'(subtree-state file-time file-size)
	dirvish-side-attributes
	'(file-size)
	dirvish-default-layout nil dirvish-mode-line-format
	'(:left
	  (sort symlink)
	  :right
	  (omit yank index))
	dirvish-preview-dispatchers
	'(image gif archive pdf)
	dirvish-preview-buffers-max-count 2 dirvish-preview-large-file-threshold
	(* 256 1024)
	dirvish-large-directory-threshold
	(and
	 (executable-find "fd")
	 10000))
  (autoload 'dirvish-override-dired-mode "dirvish" nil t)
  (dirvish-override-dired-mode)
  (with-eval-after-load 'dirvish
    (setq dirvish-quick-access-entries
	  '(("h" "~/" "Home")
	    ("d" "~/Downloads/" "Downloads")
	    ("s" "/mnt/share" "Share Drive")
	    ("t" "~/.local/share/Trash/files/" "TrashCan")))
    (global-set-key
     (kbd "C-c f")
     #'dirvish-fd)
    (define-key dirvish-mode-map
		(kbd "?")
		#'dirvish-dispatch)
    (define-key dirvish-mode-map
		(kbd "a")
		#'dirvish-quick-access)
    (define-key dirvish-mode-map
		(kbd "f")
		#'dirvish-file-info-menu)
    (define-key dirvish-mode-map
		(kbd "y")
		#'dirvish-yank-menu)
    (define-key dirvish-mode-map
		(kbd "N")
		#'dirvish-narrow)
    (define-key dirvish-mode-map
		(kbd "^")
		#'dirvish-history-last)
    (define-key dirvish-mode-map
		(kbd "h")
		#'dirvish-history-jump)
    (define-key dirvish-mode-map
		(kbd "s")
		#'dirvish-quicksort)
    (define-key dirvish-mode-map
		(kbd "v")
		#'dirvish-vc-menu)
    (define-key dirvish-mode-map
		(kbd "TAB")
		#'dirvish-subtree-toggle)
    (define-key dirvish-mode-map
		(kbd "M-f")
		#'dirvish-history-go-forward)
    (define-key dirvish-mode-map
		(kbd "M-b")
		#'dirvish-history-go-backward)
    (define-key dirvish-mode-map
		(kbd "M-l")
		#'dirvish-ls-switches-menu)
    (define-key dirvish-mode-map
		(kbd "M-m")
		#'dirvish-mark-menu)
    (define-key dirvish-mode-map
		(kbd "M-t")
		#'dirvish-layout-toggle)
    (define-key dirvish-mode-map
		(kbd "M-s")
		#'dirvish-setup-menu)
    (define-key dirvish-mode-map
		(kbd "M-e")
		#'dirvish-emerge-menu)
    (define-key dirvish-mode-map
		(kbd "M-j")
		#'dirvish-fd-jump)))


(provide '+dired-dirvish)

;;; dired-dirvish.el ends here
