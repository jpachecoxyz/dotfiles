;; Load theme first, so our eyes keep safe at startup...
(add-to-list 'load-path "~/.emacs.d/lisp/jp-themes")

(require 'jp-themes)

(defun jp/toggle-theme ()
  "Toggle between the `jp-eagle` and `jp-autumn` themes."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (if (eq current-theme 'jp-eagle)
        (progn
          ;; (disable-theme 'jp-eagle)
          (jp-themes-select 'miasma)
          (message "Dark theme loaded."))
      (progn
        (jp-themes-select 'jp-eagle)
        (message "Light theme loaded.")))))

(mapc #'disable-theme custom-enabled-themes)
;; Load the theme of choice:
;; (load-theme 'jp-darkvenom :no-confirm)
;; (load-theme 'jp-gruvby :no-confirm)
;; (load-theme 'jp-dream :no-confirm)
(load-theme 'miasma :no-confirm)
;; (load-theme 'jp-elea-dark :no-confirm)
;; (load-theme 'jp-owl :no-confirm)
(global-set-key (kbd "<f10>") 'jp/toggle-theme)

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

;; Initialize package sources
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'nano-splash)	;; Splash screen
(require 'buffer-move)   	;; Buffer-move for better window management
(require 'utilities)		;; Custom scripts
(require 'term-toggle)	;; toggle-term
