;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow company consult-denote consult-flycheck
     denote-menu denote-org denote-refs denote-silo diminish ef-themes ellama
     evil-goggles evil-nerd-commenter evil-numbers evil-surround jinx keycast
     lorem-ipsum magit marginalia mermaid-mode mu4e-views orderless org-mime
     org-tree-slide rainbow-mode typst-ts-mode uniline vertico-posframe
     yasnippet-snippets))
 '(safe-local-variable-values
   '((eval progn (setq-local org-hugo-base-dir "~/webdev/jpachecoxyz")
      (org-hugo-auto-export-mode 1))
     (eval progn
      (setq-local org-hugo-base-dir "~/.local/src/jpachecoxyz.github.io/")
      (org-hugo-auto-export-mode 1))
     (eval progn (setq-local org-hugo-base-dir "~/webdev/jpachecoxyz/")
      (org-hugo-auto-export-mode 1))
     (eval add-hook 'after-save-hook #'typst-ts-compile nil t)
     (eval add-hook 'before-save-hook #'denote-org-convert-links-to-file-type
      nil t)))
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
