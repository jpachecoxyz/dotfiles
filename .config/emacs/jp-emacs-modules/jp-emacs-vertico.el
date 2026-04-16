;;; Vertical completion layout (vertico)
(jp-emacs-configure
  (jp-emacs-install vertico)

  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

  (setq vertico-group-format
        (concat
         (propertize (make-string 20 ? ) 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator 'display '(space :align-to right))))

  (vertico-mode 1))

  (jp-emacs-keybind vertico-map
    "TAB" #'vertico-next
    "<backtab>" #'vertico-previous)

(provide 'jp-emacs-vertico)
