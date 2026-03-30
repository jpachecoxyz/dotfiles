(jp-emacs-configure
  (jp-emacs-install yasnippet)
  (jp-emacs-install yasnippet-snippets)
  ;; (jp-emacs-install ivy-yasnippet)
  
  ;; Disable yas keymap when corfu popup is visible
  (setq yas-keymap-disable-hook
        (lambda () (frame-visible-p corfu--frame)))

   ;; Load yasnippet
  (require 'yasnippet)

  ;; Enable globally
  (yas-global-mode 1) 

  ;; Enable yasnippet automatically
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)

  ;; Org TAB integration
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local yas/trigger-key [tab])
     (define-key yas-keymap [tab] #'yas-next-field-or-maybe-expand))))


(provide 'jp-emacs-yasnippets)
