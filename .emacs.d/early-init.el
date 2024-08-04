;; Load theme first, so our eyes keep safe at startup...
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/theme")
;; (load-theme 'darkvenom t)
(add-to-list 'load-path "~/.emacs.d/lisp/jp-themes")

(require 'jp-themes)

(setq ef-themes-headings
      '((1 regular 1.3)
        (2 regular 1.2)
        (3 1.1)
        (agenda-date 1)
        (agenda-structure variable-pitch regular 1)
        (t variable-pitch)))

(mapc #'disable-theme custom-enabled-themes)
;; Load the theme of choice:
(load-theme 'jp-elea-dark :no-confirm)

;; Disable GUI when foundit
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Initial message
(setq initial-scratch-message
	  ";;; -*- Calling emacs an editor is like calling the Earth a hunk of dirt.  -*- lexical-binding: t; -*-
;; --
;; It’s difficult to be rigorous about whether a machine really ’knows’, ’thinks’, etc.,
;; because we’re hard put to define these things. We understand human mental processes
;; only slightly better than a fish understands swimming.
;; --
;; <Jhon McCarthy>\n\n
;;; Code:\n")

;; (setq initial-scratch-message
;; 	  ";;; -*- Calling emacs an editor is like calling the Earth a hunk of dirt.  -*- lexical-binding: t; -*-
;; ;; --
;; ;; There are two ways to construct a software design:
;; ;; One is to make it so simple that it is obvious that there are no deficiencies,
;; ;; and the other is to make it so complicated that there are no obvious deficiencies.
;; ;; --
;; ;; <C. A. R. Hoare>\n\n
;; ;;; Code:\n")
