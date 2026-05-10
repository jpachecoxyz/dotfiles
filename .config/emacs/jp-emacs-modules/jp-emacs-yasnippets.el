(jp-emacs-configure
  (jp-emacs-install yasnippet)
  (jp-emacs-install yasnippet-snippets)
  
  ;; 1. Cargar y activar
  (require 'yasnippet)
  (yas-global-mode 1)

  ;; 2. Configuración de hooks
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)

  ;; 3. Desactivar yasnippet cuando corfu está visible para evitar conflictos
  (setq yas-keymap-disable-hook
        (lambda () (and (featurep 'corfu) 
                        (frame-visible-p corfu--frame))))

  ;; 4. Integración con Org-mode corregida
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook
              (lambda ()
                ;; Usamos la función estándar de yasnippet para expansión
                (local-set-key (kbd "<tab>") #'yas-expand)
                (local-set-key (kbd "TAB") #'yas-expand))))

  ;; 5. Configuración del keymap de navegación
  (with-eval-after-load 'yasnippet
    (define-key yas-keymap (kbd "TAB") #'yas-next-field)
    (define-key yas-keymap (kbd "<tab>") #'yas-next-field)
    
    ;; Evitar el error de auto-fill en versiones antiguas de Org
    (advice-remove 'yas--auto-fill 'org-auto-fill-function)))

(provide 'jp-emacs-yasnippets)
