;;; jp-emacs-eglot.el
;; LSP configuration

(jp-emacs-configure

 ;; eglot settings
 (setq eglot-autoshutdown t
       eglot-events-buffer-size 0
       eglot-events-buffer-config '(:size 0 :format full)
       eglot-prefer-plaintext t
       eglot-code-action-indications nil
       jsonrpc-event-hook nil)

 ;; disable jsonrpc logging
 (fset #'jsonrpc--log-event #'ignore)

 ;; workspace config example
 (setq-default
  eglot-workspace-configuration
  '(:gopls (:hints (:parameterNames t))))

 ;; setup function
 (defun jp/eglot-setup ()
   "Setup eglot except for Emacs Lisp."
   (unless (eq major-mode 'emacs-lisp-mode)
     (eglot-ensure)))

 ;; hooks
 (add-hook 'prog-mode-hook #'jp/eglot-setup)
 (add-hook 'typst-ts-mode-hook #'jp/eglot-setup)

 ;; extra servers
 (with-eval-after-load 'eglot
   (add-to-list
    'eglot-server-programs
    '((ruby-mode ruby-ts-mode) "ruby-lsp")))

 ;; keybindings
 (with-eval-after-load 'eglot
   (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
   (define-key eglot-mode-map (kbd "C-c l o") #'eglot-code-action-organize-imports)
   (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
   (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)))

(provide 'jp-emacs-eglot)
