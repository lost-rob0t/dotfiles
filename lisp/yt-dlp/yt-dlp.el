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

(defcustom nsa/music-dir (f-expand "~/Music/Inbox")
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


(defun nsa/yt-dlp-build-audio-cmd (output-string link)
  "Build a yt-dlp audio command with safe shell quoting for OUTPUT-STRING and LINK."
  (let* ((fmt (shell-quote-argument output-string))
         (url (shell-quote-argument link))
         (thumb (if nsa/music-embed-thumnail " --embed-thumbnail" "")))
    (format "yt-dlp --embed-metadata --audio-quality 0 -x --audio-format %s%s --output %s %s"
            nsa/music-format
            thumb
            fmt
            url)))

(defun nsa/dl-artist* ()
  "Download an artist, but sort songs by metadata."
  (interactive)
  (let* ((link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (artist (read-string "Artist: "))
         (output-dir (f-join nsa/music-dir "auto-sort/"))
         ;; FIXED: playlist (not playist)
         (output-string (concat output-dir
                                "%(artist,uploader|NA)s"
                                "/%(album,playlist|NA)s/"
                                "%(track,title|NA)s.%(ext)s"))
         (default-cmd (nsa/yt-dlp-build-audio-cmd output-string link))
         (cmd (read-shell-command "cmd: " default-cmd)))
    (nsa/music-append-link link genre)
    (unless (f-dir? output-dir)
      (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))

(defun nsa/dl-album* ()
  "Download an album/playlist and sort songs by metadata."
  (interactive)
  (let* ((link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsa/genres nil nil)))
         (output-dir (read-directory-name "Enter Output Dir: " nsa/music-dir))
         ;; FIXED: playlist (not playist)
         (output-string (concat output-dir
                                "%(artist,uploader|NA)s"
                                "/%(album,playlist|NA)s/"
                                "%(track,title|NA)s.%(ext)s"))
         (default-cmd (nsa/yt-dlp-build-audio-cmd output-string link))
         (cmd (read-shell-command "cmd: " default-cmd)))
    (nsa/music-append-link link genre)
    (unless (f-dir? output-dir)
      (f-mkdir-full-path output-dir))
    (nsa/async-shell-command-alert cmd "*yt-dlp*" "*yt-dlp*")))


(provide 'yt-dlp)
;;; yt-dlp.el ends here
