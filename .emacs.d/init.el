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

(setq visible-bell t)

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

;; (org-babel-load-file
;;  (expand-file-name
;;   "config.org"
;;   user-emacs-directory))

(setq custom-file "~/.emacs.d/jp-config.el")
(setq org-config-file "~/.emacs.d/config.org")

(if (file-exists-p custom-file)
    ;; If the custom file exists, load it directly
    (load custom-file)
  ;; If the custom file doesn't exist, tangle it from the Org file and then load it
  (when (file-exists-p org-config-file)
    (require 'org)
    (org-babel-tangle-file org-config-file custom-file)
    (load custom-file)))

;; Fonts settings.

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 130
                    :weight 'regular)

;; Set italic font
(set-face-attribute 'italic nil
                    :family "Iosevka"
                    :height 130
                    :slant 'italic)

;; Set bold font
(set-face-attribute 'bold nil
                    :family "Iosevka"
                    :height 130
                    :weight 'bold)

;; ;; Set bold-italic font
;; (set-face-attribute 'bold-italic nil
;;                     :family "JetBrains Mono"
;;                     :slant 'italic)

;; Set font for comments to be italic
(set-face-attribute 'font-lock-comment-face nil
                    :family "IBM Iosevka"
                    :slant 'italic)

;; Optionally, also set italic for doc comments
(set-face-attribute 'font-lock-doc-face nil
                    ;; :family "IBM Plex Mono"
                    :family "Iosevka"
                    :slant 'italic)

;; Set monospaced font for code and programming modes
(set-face-attribute 'org-block nil
                    ;; :family "IBM Plex Mono"
                    :family "Iosevka"
                    :height 120)

;; Optionally, set the code block font (Org-mode source blocks, markdown, etc.)
(set-face-attribute 'org-verbatim nil
                    ;; :family "IBM Plex Mono"
                    :family "Iosevka"
					:slant 'italic
                    :height 130)


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
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(org-checkbox ((t (:box (:style released-button)))))
 '(org-checkbox-statistics-done ((t (:inherit org-todo))))

 ;; Enable org-crypt and epa-file
 (require 'org-crypt)
 (require 'epa-file)

 ;; Enable epa-file for encryption/decryption of files
 (epa-file-enable)

 (setq epa-file-encrypt-to '("jpacheco@cock.li"))  ; Replace with your GPG key email

 ;; Configure org-crypt
 (setq org-crypt-tag-matcher "crypt")                    ; Tag used to encrypt entries
 (setq org-crypt-key "jpacheco@cock.li")          ; Replace with your GPG key email
 (setq org-tags-exclude-from-inheritance '("crypt"))     ; Prevent inheritance of "crypt" tag
 (setq epa-pinentry-mode 'loopback) ;; Ensures passphrase prompt in minibuffer

 ;; Automatically encrypt entries tagged with "crypt" before saving
 (add-hook 'before-save-hook 'org-crypt-use-before-save-magic)

 ;; Define keybindings for manual encryption/decryption
 (define-key org-mode-map (kbd "C-c C-x e") 'org-encrypt-entries)
 (define-key org-mode-map (kbd "C-c C-x d") 'org-decrypt-entries)
 )
