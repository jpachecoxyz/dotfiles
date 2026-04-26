;;; The Doric themes

(jp-emacs-configure
  (jp-emacs-install doric-themes)

  (setq doric-themes-variable-pitch-ui t
        doric-themes-mixed-fonts t
        doric-themes-bold-constructs t
        doric-themes-italic-constructs t
        doric-themes-to-rotate nil ; defaults to the return value of `modus-themes-get-themes'
        doric-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.8))
          (1 . (variable-pitch light 1.0))
          (2 . (variable-pitch regular 1.0))
          (3 . (variable-pitch regular 1.0))
          (4 . (variable-pitch regular 1.0))
          (5 . (variable-pitch 1.0)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.0))
          (7 . (variable-pitch 1.0))
          (agenda-date . (semilight 1.0))
          (agenda-structure . (variable-pitch light 1.0))
          (t . (variable-pitch 1.0))))

  (jp-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)
  (load-theme 'doric-fire))

(provide 'jp-emacs-doric-themes)
