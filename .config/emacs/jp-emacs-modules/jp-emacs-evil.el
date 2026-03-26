;;; Evil-mode

(jp-emacs-configure
  (jp-emacs-install evil)
  (evil-mode 1))

  (with-eval-after-load 'org
    (evil-define-key '(normal insert) org-mode-map
        (kbd "TAB") #'org-cycle
        (kbd "<backtab>") #'org-shifttab))

  (setq evil-want-integration t)  ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-mode-line-format nil)
  (setq evil-undo-system 'undo-redo) 

(jp-emacs-configure
  (jp-emacs-install evil-collection))

(jp-emacs-configure
  (jp-emacs-install evil-nerd-commenter))

(jp-emacs-configure
  (jp-emacs-install evil-numbers))

(jp-emacs-configure
  (jp-emacs-install evil-surround))

(with-eval-after-load 'evil
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init)

  (global-evil-surround-mode 1)

  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-s") #'evil-numbers/dec-at-pt))

;; Using RETURN to follow links in Org/Evil
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(provide 'jp-emacs-evil)
