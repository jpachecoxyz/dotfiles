;;; Icons
;; NOTE 2025-12-21: Also see the `jp-minibuffer.el' and my
;; `completion-category-overrides' for completion-related icons.
(jp-emacs-configure
  (jp-emacs-install all-the-icons-dired)
  
  ;; (require 'jp-icons)

  ;; (jp-icons-dired-mode 1)
  ;; (jp-icons-xref-mode 1)
  ;; 
  ;; (when jp-emacs-load-theme-family
  ;;   (cond
  ;;    ((memq jp-emacs-load-theme-family '(modus ef standard))
  ;;     (defun jp/icons-set-faces ()
  ;;       (modus-themes-with-colors
  ;;         (custom-set-faces
  ;;          `(jp-icons-icon ((,c :inherit (bold fixed-pitch) :box (:line-width (1 . -1)) :inverse-video t)))
  ;;          `(jp-icons-directory ((,c :inherit bold :foreground ,accent-0)))
  ;;          `(jp-icons-gray ((,c :inherit bold :foreground ,fg-dim)))
  ;;          `(jp-icons-red ((,c :inherit jp-icons-icon :foreground ,red)))
  ;;          `(jp-icons-green ((,c :inherit jp-icons-icon :foreground ,green-warmer)))
  ;;          `(jp-icons-yellow ((,c :inherit jp-icons-icon :foreground ,yellow)))
  ;;          `(jp-icons-blue ((,c :inherit jp-icons-icon :foreground ,blue-cooler)))
  ;;          `(jp-icons-magenta ((,c :inherit jp-icons-icon :foreground ,magenta-cooler)))
  ;;          `(jp-icons-cyan ((,c :inherit jp-icons-icon :foreground ,cyan))))))
  ;;     (add-hook 'modus-themes-after-load-theme-hook #'jp/icons-set-faces))
  ;;    ((eq jp-emacs-load-theme-family 'doric)
  ;;     (defun jp/icons-set-faces ()
  ;;       (doric-themes-with-colors
  ;;        (custom-set-faces
  ;;         `(jp-icons-icon ((t :inherit (bold fixed-pitch) :box (:line-width (1 . -1)) :inverse-video t)))
  ;;         `(jp-icons-directory ((t :inherit bold :foreground ,fg-accent)))
  ;;         `(jp-icons-gray ((t :inherit bold :foreground ,fg-shadow-subtle)))
  ;;         `(jp-icons-red ((t :inherit jp-icons-icon :foreground ,fg-red)))
  ;;         `(jp-icons-green ((t :inherit jp-icons-icon :foreground ,fg-green)))
  ;;         `(jp-icons-yellow ((t :inherit jp-icons-icon :foreground ,fg-yellow)))
  ;;         `(jp-icons-blue ((t :inherit jp-icons-icon :foreground ,fg-blue)))
  ;;         `(jp-icons-magenta ((t :inherit jp-icons-icon :foreground ,fg-magenta)))
  ;;         `(jp-icons-cyan ((t :inherit jp-icons-icon :foreground ,fg-cyan))))))
  ;;     (add-hook 'doric-themes-after-load-theme-hook #'jp/icons-set-faces)))
  ;; 
  ;;   (jp/icons-set-faces)))
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

(provide 'jp-emacs-icons)
