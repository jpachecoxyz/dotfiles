(jp-emacs-configure
  (jp-emacs-install yasnippet)
  (jp-emacs-install yasnippet-snippets)
  
  ;; 1. Cargar y activar globalmente
  (require 'yasnippet)
  (yas-global-mode 1)

  ;; 2. Hooks limpios
  ;; yas-global-mode ya se encarga de la mayoría, pero reforzamos org
  (add-hook 'org-mode-hook #'yas-minor-mode)

  ;; 3. Evitar conflictos con Corfu (Corregido)
  ;; Es mejor usar el hook de corfu si está disponible
  (with-eval-after-load 'corfu
    (add-hook 'yas-before-expand-snippet-hook (lambda () (corfu-quit))))

  ;; 4. Integración con Org-mode (LA FORMA CORRECTA)
  (with-eval-after-load 'org
    (defun my/org-tab-conditional ()
      (interactive)
      (if (and (bound-and-true-p yas-minor-mode)
               (yas-expand))
          (message "Snippet expandido")
        (org-cycle))) ; Si no hay snippet, hace el TAB normal de Org

    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key (kbd "<tab>") #'my/org-tab-conditional)
                (local-set-key (kbd "TAB") #'my/org-tab-conditional))))

  ;; 5. Navegación dentro del snippet
  (with-eval-after-load 'yasnippet
    (define-key yas-keymap (kbd "TAB") #'yas-next-field)
    (define-key yas-keymap (kbd "<tab>") #'yas-next-field)
    (define-key yas-keymap (kbd "S-TAB") #'yas-prev-field)
    (define-key yas-keymap (kbd "<backtab>") #'yas-prev-field))

  ;; 6. Limpieza de errores conocidos
  (setq yas-verbosity 1) ; Evita mensajes molestos en el echo area
  )

(provide 'jp-emacs-yasnippets)
