;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aggressive-indent all-the-icons-completion all-the-icons-dired
                       all-the-icons-ivy auctex auto-package-update buffer-flip
                       bufferfile cape company-box consult-denote consult-dir
                       consult-flycheck corfu counsel-projectile dap-mode
                       dashboard denote-explore denote-menu denote-org
                       denote-regexp denote-search denote-sequence denote-silo
                       diminish dired-open doom-modeline eldoc-box elixir-mode
                       elixir-ts-mode ellama embark-consult evil-collection
                       evil-goggles evil-nerd-commenter evil-numbers evil-owl
                       evil-surround evil-terminal-cursor-changer
                       fill-column-indicator flymake-ruff flymake-shellcheck
                       form-feed fzf general git-gutter-fringe git-timemachine
                       hide-lines hide-mode-line highlight-indent-guides
                       highlight-thing hl-todo htmlize ivy-yasnippet jinx
                       justify-kp key-chord keycast kind-icon kv ligature
                       lorem-ipsum magit major-mode-hydra marginalia
                       markdown-mode nerd-icons-completion no-littering
                       nordic-night-theme nov olivetti orderless org-auto-tangle
                       org-bullets org-contacts org-download org-mime org-modern
                       org-rainbow-tags org-sidebar org-tree-slide ox-hugo
                       page-break-lines paredit password-store pdf-tools
                       peep-dired pipenv popper pulsar python-mode
                       rainbow-delimiters rainbow-mode rust-mode scss-mode
                       shackle shell-pop simple-httpd slime spinner telega
                       toc-org tree-sitter-langs treemacs treesit-auto
                       treesit-ispell tsx-ts-mode undohist uv-menu
                       vertico-posframe vterm vundo xml+ yaml yasnippet-capf
                       yasnippet-snippets))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'denote-org-convert-links-to-file-type nil t)))
 '(smtpmail-smtp-server "mail.cock.li")
 '(smtpmail-smtp-service 25))
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
 '(fill-column-indicator ((t (:background "#222222" :foreground "white" :height 1))))
 '(tab-bar ((((class color) (min-colors 256)) :background "#232635" :foreground "#A6Accd")))
 '(tab-bar-tab ((((class color) (min-colors 256)))))
 '(tab-bar-tab-inactive ((((class color) (min-colors 256))))))
