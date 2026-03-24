;;; jp-emacs-treesit.el
;; Tree-sitter configuration

(jp-emacs-configure
  (jp-emacs-install treesit-auto)
  (require 'treesit-auto)
   (treesit-auto-add-to-auto-mode-alist 'all)
   (global-treesit-auto-mode t))

(provide 'jp-emacs-treesitter)
