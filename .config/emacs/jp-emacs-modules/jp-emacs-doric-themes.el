;;; The Doric themes

(jp-emacs-configure
  (jp-emacs-install doric-themes)

  (jp-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (jp-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

;; For testing purposes
(jp-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/doric-themes/")

  (require 'doric-themes)

  (jp-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (jp-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

(provide 'jp-emacs-doric-themes)
