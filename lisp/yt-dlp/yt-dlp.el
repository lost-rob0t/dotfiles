;;; yt-dlp-music-.el --- Download music   -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@airmail.cc>
;; Maintainer:  <nsaspy@airmail.cc>
;; Created: July 02, 2023
;; Modified: January 24, 2025
;; Version: 0.3.0
;; Keywords: music yt-dlp youtube
;; Homepage: https://github.com/lost-rob0t/yt-dlp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'async)
(require 's)
(require 'f)

(defcustom nsa/music-dir (f-expand "~/usb/Music/")
  "path to music dir.")
(defcustom nsa/genres '("psytrance" "retrowave" "misc")
  "Default list of music genre to use")

(defcustom nsa/music-format "mp3"
  "Format of the music")
(defcustom nsa/music-embed-thumnail t
  "Weather to embed the thumnail.")



(defun nsa/music-append-link (link genre)
  "Append a LINK to the Genre  links file so we dont lose it."
  (with-temp-buffer
    (insert (format "%s\n" link))
    (append-to-file (point-min) (point-max) (f-join nsa/music-dir (format "%s.links" genre)))))


(defun nsa/dl-song ()
  "Download a song."
  (interactive)
  (let* (
         (link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (output-dir (f-join nsa/music-dir genre))
         (output-string (concat output-dir "/%(title)s.%(ext)s"))

         (format-string (shell-quote-argument output-string))

         (cmd (read-shell-command "cmd: " (format "yt-dlp --audio-quality 0 -x --audio-format %s --embed-thumbnail --output %s %s --embed-metadata"
                                                  nsa/music-format format-string link))))

    (nsa/music-append-link link genre)
    (if (not (f-dir? output-dir))
        (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))


(defun nsa/dl-artist ()
  "Download a artist. Highly recomended to find the artist's releases page."
  (interactive)
  (let* (
         (link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (artist (read-string "Artist: "))
         (output-dir (f-join nsa/music-dir genre))
         (output-string (concat output-dir "/" artist "/%(playlist)s/%(title)s.%(ext)s"))

         (format-string (shell-quote-argument output-string))

         (cmd (read-shell-command "cmd: " (format "yt-dlp --audio-quality 0 -x --audio-format %s --embed-thumbnail --output %s %s --embed-metadata"
                                                  nsa/music-format format-string link))))

    (nsa/music-append-link link genre)
    (if (not (f-dir? output-dir))
        (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))

(defun nsa/dl-artist* ()
  "Download a artist, but sort songs by metadata."
  (interactive)
  (let* (
         (link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (artist (read-string "Artist: "))
         (output-dir (f-join nsa/music-dir "auto-sort/"))
         (output-string (concat output-dir "%(artist,uploader|NA)s" "/%(album,playist|NA)s/%(track,title|NA)s.%(ext)s"))

         (format-string (shell-quote-argument output-string))

         (cmd (read-shell-command "cmd: " (format "yt-dlp --audio-quality 0 -x --audio-format %s --embed-thumbnail --output %s %s --embed-metadata"
                                                  nsa/music-format format-string link))))

    (nsa/music-append-link link genre)
    (if (not (f-dir? output-dir))
        (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))



(defun nsa/remove-duplicate-titles ()
  "Remove duplicate song titles in artist - title.song format format in subdirectories."
  (interactive)
  (let* ((root-dir (read-directory-name "Enter root directory: "))
         (files (directory-files-recursively root-dir (format "\\.%s$" nsa/music-format)))
         (titles-alist (make-hash-table :test 'equal))
         (duplicates '()))
    ;; Iterate over each file
    (dolist (file files)
      (when (string-match (format  "\\([^/]+\\)/\\([^/]+\\) - \\([^/]+\\)\\.%s$" nsa/music-format) file)
        (let* ((artist (match-string 2 file))
               (title (match-string 3 file))
               (key (concat artist " - " title)))
          (if (gethash key titles-alist)
              (push file duplicates)
            (puthash key t titles-alist)))))
    ;; Delete duplicate files
    (dolist (duplicate duplicates)
      (delete-file duplicate))
    (message "Removed %d duplicate song titles." (length duplicates))))


(defun nsa/dl-album* ()
  "Download a artist, but sort songs by metadata."
  (interactive)
  (let* (
         (link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (output-dir (read-directory-name "Enter Output Dir: " nsa/music-dir))

         (output-string (concat output-dir "%(artist,uploader|NA)s" "/%(album,playist|NA)s/%(track,title|NA)s.%(ext)s"))

         (format-string (shell-quote-argument output-string))

         (cmd (read-shell-command "cmd: " (format "yt-dlp --embed-metadata --audio-quality 0 -x --audio-format %s --embed-thumbnail --output %s %s "
                                                  nsa/music-format format-string link))))

    (nsa/music-append-link link genre)
    (if (not (f-dir? output-dir))
        (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))




(provide 'yt-dlp)
;;; yt-dlp.el ends here
