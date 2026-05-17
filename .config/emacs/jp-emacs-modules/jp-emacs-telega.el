(jp-emacs-configure
  (jp-emacs-install telega)
  
  (setq telega-chat-input-markups '("markdown2" "org" nil))
  
  (setq telega-completing-read-function #'completing-read)
  
  (define-key telega-chat-mode-map (kbd "S-<return>") #'newline)
  
  (with-eval-after-load 'telega
    (define-key telega-msg-button-map (kbd "l") nil))
  
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (setq-local show-trailing-whitespace nil))))

(defun my/start-telega ()
  "Start `telega' inside a new perspective and activate 'telega-mode-line-mode'"
  (interactive)
  (if (fboundp 'persp-switch)
      (persp-switch "*telega*")
    (switch-to-buffer "*telega*"))
  (telega)
  (telega-mode-line-mode))

(provide 'jp-emacs-telega)
