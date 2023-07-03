;;; yt-dlp-music-.el --- Download music   -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@airmail.cc>
;; Maintainer:  <nsaspy@airmail.cc>
;; Created: July 02, 2023
;; Modified: July 02, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
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

(defcustom nsaspy/music-dir (f-expand "~/usb/Music/")
  "path to music dir.")
(defcustom nsaspy/genres '("psytrance" "retrowave" "misc")
  "Default list of music genre to use")

(defcustom nsaspy/music-format "mp3"
  "Format of the music")
(defcustom nsaspy/music-embed-thumnail t
  "Weather to embed the thumnail.")



(defun nsaspy/music-append-link (link genre)
  "Append a LINK to the Genre  links file so we dont lose it."
  (with-temp-buffer
    (insert (format "%s\n" link))
    (append-to-file (point-min) (point-max) (f-join nsaspy/music-dir (format "%s.links" genre)))))


(defun nsaspy/dl-song ()
  "Download a song."
  (interactive)
  (let* (
         (link (read-string "Url: " (current-kill 0)))
         (genre (downcase (completing-read "genre: " nsaspy/genres nil nil)))
         (output-dir (f-join nsaspy/music-dir genre))
         (output-string (concat output-dir "/%(title)s.%(ext)s"))

         (format-string (shell-quote-argument output-string))

         (cmd (read-string "cmd: " (format "yt-dlp -x --audio-format %s --embed-thumbnail --output %s %s"
                                           nsaspy/music-format format-string link))))

    (nsaspy/music-append-link link genre)
    (if (not (f-dir? output-dir))
        (f-mkdir-full-path output-dir))
    (async-shell-command cmd "*yt-dlp*" "*yt-dlp*")))



(provide 'yt-dlp)
;;; yt-dlp.el ends here
