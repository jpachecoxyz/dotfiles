(jp-emacs-configure

  ;; install packages
  (jp-emacs-install lsp-mode)
  (jp-emacs-install lsp-ui)
  (jp-emacs-install lsp-treemacs)

  ;; start lsp automatically
  (add-hook 'prog-mode-hook #'lsp-deferred)

  ;; performance tweaks
  (setq lsp-idle-delay 0.2)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)

  ;; disable things that slow lsp
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)

  ;; ui
  (with-eval-after-load 'lsp-ui
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-position 'at-point)
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-code-actions t))

)

(provide 'jp-emacs-lsp)
