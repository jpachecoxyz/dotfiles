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
    "Enable Eglot in programming modes except Emacs Lisp."
    (unless (derived-mode-p 'emacs-lisp-mode)
        (eglot-ensure)))

    ;; Add lsp servers to eglot
    (with-eval-after-load 'eglot
    ;; Bash LSP, Starts 'bash-language-server' with 'start' argument.
    ;; Example: $ bash-language-server start
        (add-to-list 'eglot-server-programs
                    '(sh-mode "bash-language-server" "start"))

        ;; Python LSP
        (add-to-list 'eglot-server-programs
                    '(python-mode "pylsp")))


 ;; hooks
    (add-hook 'prog-mode-hook #'jp/eglot-setup)
    (add-hook 'shell-mode-hook #'eglot-ensure)

 ;; keybindings
 (with-eval-after-load 'eglot
   (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
   (define-key eglot-mode-map (kbd "C-c l o") #'eglot-code-action-organize-imports)
   (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
   (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)))

;;; Python
;; by default, the function 'python-mode is associated with
;; the package python.el. The following changes that to python-mode.el:
(autoload 'python-mode "python-mode" "Python Mode." t)

;; open py files with python-mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; sets python interpreter mode to be python-mode
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(provide 'jp-emacs-eglot)
