;;; jp-emacs-treesit.el
;; Tree-sitter configuration

(jp-emacs-configure
  (jp-emacs-install treesit-auto)
    (setq treesit-auto-install t))

(provide 'jp-emacs-treesitter)
