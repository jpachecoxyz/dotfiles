;;; Evil-mode

(jp-emacs-configure
  (jp-emacs-install evil)
  (evil-mode 1))

  (defun jp/org-tab-dwim ()
    (interactive)
      (or (yas-expand)
        (org-cycle)))

  (with-eval-after-load 'org
    (evil-define-key '(normal insert) org-mode-map
        (kbd "TAB") #'jp/org-tab-dwim
        (kbd "<backtab>") #'org-shifttab))

  (with-eval-after-load 'evil
    (evil-define-key '(normal insert) lisp-interaction-mode-map
        (kbd "C-j") #'jp-elisp-eval-and-print-last-sexp))

  ;; (setq evil-want-integration t)  ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-mode-line-format nil)
  (setq evil-undo-system 'undo-redo) 

(jp-emacs-configure
  (jp-emacs-install evil-collection)
  (evil-collection-init))

(jp-emacs-configure
  (jp-emacs-install evil-nerd-commenter))

(jp-emacs-configure
  (jp-emacs-install evil-numbers))

(jp-emacs-configure
  (jp-emacs-install evil-surround))

(with-eval-after-load 'evil
  ;; (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init)

  (global-evil-surround-mode 1)

  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-s") #'evil-numbers/dec-at-pt))

;; Using RETURN to follow links in Org/Evil
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

 ;; Dired vim-like navigation
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "h") #'dired-up-directory
    (kbd "l") #'dired-find-file))

(provide 'jp-emacs-evil)
