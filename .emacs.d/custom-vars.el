;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-styles '(partial-completion flex initials) nil nil "Customized with use-package minibuffer")
 '(package-selected-packages
   '(all-the-icons-completion auctex auto-package-update company consult-denote
							  consult-dir consult-flycheck denote-explore
							  denote-menu denote-sequence denote-silo diminish
							  dired-open dired-preview evil-collection
							  evil-goggles evil-nerd-commenter evil-numbers
							  evil-owl evil-surround
							  evil-terminal-cursor-changer form-feed fzf general
							  git-timemachine hide-lines hide-mode-line
							  highlight-indent-guides highlight-thing
							  ivy-yasnippet jinx key-chord keycast lorem-ipsum
							  magit marginalia nerd-icons-completion
							  no-littering orderless org-auto-tangle org-bullets
							  org-download org-mime org-rainbow-tags org-sidebar
							  org-tree-slide ox-hugo ox-typst page-break-lines
							  pdf-tools peep-dired popper posframe pulsar
							  rainbow-delimiters rainbow-mode scss-mode shackle
							  toc-org treesit-auto treesit-ispell typst-preview
							  typst-ts-mode undohist vertico websocket
							  yasnippet-snippets))
 '(package-vc-selected-packages
   '((typst-ts-mode :url "https://codeberg.org/meow_king/typst-ts-mode/")
	 (typst-preview :url "https://github.com/havarddj/typst-preview.el")))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'denote-org-convert-links-to-file-type nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(org-block ((((class color) (min-colors 256)) :background "#2f2f2f")))
 '(org-block-begin-line ((((class color) (min-colors 256)) :background "#222222")))
 '(org-block-end-line ((((class color) (min-colors 256)) :background "#222222")))
 '(tab-bar ((((class color) (min-colors 256)) :background "#232635" :foreground "#A6Accd")))
 '(tab-bar-tab ((((class color) (min-colors 256)))))
 '(tab-bar-tab-inactive ((((class color) (min-colors 256))))))
