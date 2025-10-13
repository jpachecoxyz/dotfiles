;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company consult-denote consult-flycheck denote-menu denote-org denote-refs
     denote-sequence denote-silo diminish ellama evil-goggles
     evil-nerd-commenter evil-numbers evil-surround jinx keycast lorem-ipsum
     magit marginalia mermaid-mode orderless org-mime org-tree-slide
     rainbow-mode typst-ts-mode vertico-posframe yasnippet-snippets))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'denote-org-convert-links-to-file-type nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
