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

;;; Custom tweaks for vertico (jp-vertico.el)
(jp-emacs-configure
  (require 'jp-vertico)
  (setq vertico-multiform-commands
        `(("consult-\\(.*\\)?\\(find\\|grep\\|ripgrep\\)" ,@jp-vertico-multiform-maximal)))
  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@jp-vertico-multiform-maximal)
          (multi-category ,@jp-vertico-multiform-maximal)
          (consult-location ,@jp-vertico-multiform-maximal)
          (imenu ,@jp-vertico-multiform-maximal)
          (theme ,@jp-vertico-multiform-maximal)
          (unicode-name ,@jp-vertico-multiform-maximal)
          ;; Minimal
          (file ,@jp-vertico-multiform-minimal
                (vertico-sort-function . vertico-sort-directories-first))
          (t ,@jp-vertico-multiform-minimal)))

  (vertico-multiform-mode 1)

  (jp-emacs-keybind vertico-map
    "<left>" #'backward-char
    "<right>" #'forward-char
    "C-M-n" #'vertico-next-group
    "C-M-p" #'vertico-previous-group
    "TAB" #'jp-vertico-private-complete
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit)
  (jp-emacs-keybind vertico-multiform-map
    "RET" #'jp-vertico-private-exit
    "<return>" #'jp-vertico-private-exit
    "C-n" #'jp-vertico-private-next
    "<down>" #'jp-vertico-private-next
    "C-p" #'jp-vertico-private-previous
    "<up>" #'jp-vertico-private-previous
    "C-l" #'vertico-multiform-vertical))

(provide 'jp-emacs-vertico)
