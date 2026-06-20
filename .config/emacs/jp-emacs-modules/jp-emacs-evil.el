;;; Evil-model

(jp-emacs-configure
  (jp-emacs-install evil)
  (evil-mode 1))

(defun jp/org-tab-dwim ()
  (interactive)
  (or (yas-expand)
      (org-cycle)))

(with-eval-after-load 'evil
  (evil-define-key '(normal insert) lisp-interaction-mode-map
    (kbd "C-j") #'jp-elisp-eval-and-print-last-sexp))

;;; Dired Binds
(with-eval-after-load 'dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" 'dired-find-file
    "h" 'dired-up-directory))

(with-eval-after-load 'org
  (evil-define-key '(normal insert) org-mode-map
    (kbd "TAB") #'jp/org-tab-dwim
    (kbd "<backtab>") #'org-shifttab))

(with-eval-after-load 'org-agenda
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "<tab>")     #'org-agenda-next-item
    (kbd "TAB")       #'org-agenda-next-item
    (kbd "<backtab>") #'org-agenda-previous-item
    (kbd "S-TAB")     #'org-agenda-previous-item))

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

(setq evil-normal-state-tag   (propertize " Normal " 'face 'jp-modeline-indicator-cyan-bg)
      evil-insert-state-tag   (propertize " Insert " 'face 'jp-modeline-indicator-yellow-bg)
      evil-visual-state-tag   (propertize " Visual " 'face 'jp-modeline-indicator-red-bg)
      evil-motion-state-tag   (propertize " Motion " 'face 'jp-modeline-indicator-cyan-bg)
      evil-replace-state-tag  (propertize " Replace " 'face 'jp-modeline-indicator-green-bg)
      evil-emacs-state-tag    (propertize " Emacs " 'face 'jp-modeline-indicator-magenta-bg)
      evil-operator-state-tag (propertize " Operator " 'face 'jp-modeline-indicator-blue-bg))

(setq evil-insert-state-message nil
      evil-visual-state-message nil
      evil-replace-state-message nil
      evil-motion-state-message nil)

(provide 'jp-emacs-evil)
