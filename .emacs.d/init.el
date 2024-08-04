(setq byte-compile-warnings '(cl-functions))
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
		 (tab-width (or width tab-width))
		 (count (/ max-column tab-width)))
	(number-sequence tab-width (* tab-width count) tab-width)))

(setq blink-cursor-mode t)
(setq ring-bell-function 'ignore)
(setq tab-stop-list (my-generate-tab-stops))
(put 'outline-forward-same-level 'disabled t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)

(setq visible-bell 1)
(setq-default current-language-environment "English")
(setq keyboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default truncate-lines t)
(setq-default fill-column 80)
(setq line-move-visual t) ;; C-p, C-n, etc uses visual lines
(setq-default display-line-numbers-width 3)

;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq package-install-upgrade-built-in t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun jp/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jp/display-startup-time)
(add-hook 'emacs-startup-hook 'blink-cursor-mode)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(corfu-doc yasnippet-snippets yasnippet-capf which-key vundo vertico-posframe treesit-ispell tree-sitter-langs toc-org telega shackle rainbow-mode rainbow-delimiters python-mode pulsar pretty-mode pipenv peep-dired pdf-tools password-store parrot page-break-lines ox-hugo org-sidebar org-roam-ui org-mime org-fancy-priorities org-download org-contacts org-bullets org-auto-tangle orderless olivetti nyan-mode no-littering nix-mode nerd-icons-completion marginalia major-mode-hydra magit lsp-ui lsp-pyright ligature kind-icon key-chord ivy-yasnippet htmlize hl-todo highlight-thing highlight-indent-guides git-timemachine git-gutter-fringe general form-feed fontaine flyspell-correct-ivy evil-surround evil-owl evil-goggles evil-collection doom-modeline dired-open diminish dap-mode counsel-projectile corfu consult-flycheck consult-dir command-log-mode cape calfw-org calfw buffer-flip auto-package-update auctex all-the-icons-dired all-the-icons-completion aggressive-indent)))
