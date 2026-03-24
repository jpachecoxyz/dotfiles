;;; Evil-mode

(jp-emacs-configure
  (jp-emacs-install evil)
  (evil-mode 1))

  (setq evil-want-integration t)  ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-mode-line-format nil)
  (setq evil-undo-system 'undo-redo) 

(provide 'jp-emacs-evil)
