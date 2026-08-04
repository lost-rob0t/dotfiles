;;; swiper.el -*- lexical-binding: t; -*-

;;;###autoload
(map! :leader
      :desc "Swiper search buffer"
      "s s" #'swiper-isearch)
