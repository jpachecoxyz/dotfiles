;;; init.el --- Emacs (no external packages) Configuration --- Init  -*- lexical-binding: t; -*-
;;
;; Author: Javier Pacheco
;; URL: jpachecoxyz.github.io
;; Keywords: config
;;

;;; Commentary:
;;  Init configuration for Emacs
;;

;;; Code:

;;; -------------------- EMACS USTOM OPTIONS
;;
;;  Some features Emacs Solo provides you can turn on/off
(defcustom emacs-solo-enable-transparency t
  "Enable `emacs-solo-transparency'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-dired-icons t
  "Enable `emacs-solo-dired-icons'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-olivetti t
  "Enable `emacs-solo-olivetti'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-dired-gutter t
  "Enable `emacs-solo-enable-dired-gutter'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-highlight-keywords t
  "Enable `emacs-solo-enable-highlight-keywords'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-rainbown-delimiters t
  "Enable `emacs-solo-enable-rainbown-delimiters'."
  :type 'boolean
  :group 'emacs-solo)

(defcustom emacs-solo-enable-buffer-gutter t
  "Enable `emacs-solo-enable-gutter'."
  :type 'boolean
  :group 'emacs-solo)


;;; -------------------- GENERAL EMACS CONFIG
;;; EMACS
(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("M-j" . duplicate-dwim)
   ("M-g r" . recentf)
   ("M-s g" . grep)
   ("C-x ;" . comment-line)
   ("M-s f" . find-name-dired)
   ("C-x C-b" . ibuffer)
   ("C-x w t"  . window-layout-transpose)            ; EMACS-31
   ("C-x w r"  . window-layout-rotate-clockwise)     ; EMACS-31
   ("C-x w f h"  . window-layout-flip-leftright)     ; EMACS-31
   ("C-x w f v"  . window-layout-flip-topdown)       ; EMACS-31
   ("C-x 5 l"  . select-frame-by-name)
   ("C-x 5 s"  . set-frame-name)
   ("RET" . newline-and-indent)
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil))
  :custom
  (ad-redefinition-action 'accept)
  (column-number-mode nil)
  (line-number-mode nil)
  (line-spacing nil)
  (completion-ignore-case t)
  (completions-detailed t)
  (delete-by-moving-to-trash t)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  (delete-selection-mode 1)
  (enable-recursive minibuffers t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (help-window-select t)
  (history-length 300)
  (inhibit-startup-message t)
  (fset 'yes-or-no-p 'y-or-n-p) ;; never type: yes or no
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (kill-do-not-save-duplicates t)
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (read-answer-short t)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (resize-mini-windows 'grow-only)
  (ring-bell-function #'ignore)
  (scroll-conservatively 8)
  (scroll-margin 5)
  (savehist-save-minibuffer-history t)    ; t is default
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
     register-alist                       ; macros
     mark-ring global-mark-ring           ; marks
     search-ring regexp-search-ring))     ; searches
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  (set-mark-command-repeat-pop t) ; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
  (split-width-threshold 170)     ; So vertical splits are preferred
  (split-height-threshold nil)
  (shr-use-colors nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-package-hook-name-suffix nil)
  (use-short-answers t)
  (visible-bell nil)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  (xref-search-program 'ripgrep)
  (zone-programs '[zone-pgm-rat-race])
  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  :config
  ;; Makes everything accept utf-8 as default, so buffers with tsx and so
  ;; won't ask for encoding (because undecided-unix) every single keystroke
  (modify-coding-system-alist 'file "" 'utf-8)

  (set-face-attribute 'default nil :family "Iosevka" :height 120)

  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")
    (setq mac-command-modifier 'meta)
    (set-face-attribute 'default nil :family "Iosevka" :height 130))

  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Set line-number-mode with relative numbering
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; A Protesilaos life savier HACK
  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff (if you choose `d') of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; On Terminal: changes the vertical separator to a full vertical line
  ;;              and truncation symbol to a right arrow
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)

  ;; Ibuffer filters
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("org"     (or
                       (mode . org-mode)
                       (name . "^\\*Org Src")
                       (name . "^\\*Org Agenda\\*$")))
           ("tramp"   (name . "^\\*tramp.*"))
           ("emacs"   (or
                       (name . "^\\*scratch\\*$")
                       (name . "^\\*Messages\\*$")
                       (name . "^\\*Warnings\\*$")
                       (name . "^\\*Shell Command Output\\*$")
                       (name . "^\\*Async-native-compile-log\\*$")))
           ("ediff"   (name . "^\\*[Ee]diff.*"))
           ("vc"      (name . "^\\*vc-.*"))
           ("dired"   (mode . dired-mode))
           ("terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eshell-mode)))
           ("help"    (or
                       (name . "^\\*Help\\*$")
                       (name . "^\\*info\\*$")))
           ("news"    (name . "^\\*Newsticker.*"))
           ("gnus"    (or
                       (mode . message-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\*Group\\*")
                       (name . "^\\*Summary\\*")
                       (name . "^\\*Article\\*")
                       (name . "^\\*BBDB\\*")))
           ("chat"    (or
                       (mode . rcirc-mode)
                       (mode . erc-mode)
                       (name . "^\\*rcirc.*")
                       (name . "^\\*ERC.*"))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups


  ;; So eshell git commands open an instance of THIS config of Emacs
  (setenv "GIT_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (setenv "EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  ;; So rebase from eshell opens with a bit of syntax highlight
  (add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode))


  ;; Runs 'private.el' after Emacs inits

  :init
  (set-window-margins (selected-window) 2 0)

  (select-frame-set-input-focus (selected-frame))
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one

  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;
;;      ██╗██████╗  █████╗  ██████╗██╗  ██╗███████╗ ██████╗ ██████╗ ██╗  ██╗██╗   ██╗███████╗
;;      ██║██╔══██╗██╔══██╗██╔════╝██║  ██║██╔════╝██╔════╝██╔═══██╗╚██╗██╔╝╚██╗ ██╔╝╚══███╔╝
;;      ██║██████╔╝███████║██║     ███████║█████╗  ██║     ██║   ██║ ╚███╔╝  ╚████╔╝   ███╔╝
;; ██   ██║██╔═══╝ ██╔══██║██║     ██╔══██║██╔══╝  ██║     ██║   ██║ ██╔██╗   ╚██╔╝   ███╔╝
;; ╚█████╔╝██║     ██║  ██║╚██████╗██║  ██║███████╗╚██████╗╚██████╔╝██╔╝ ██╗   ██║   ███████╗
;;  ╚════╝ ╚═╝     ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝
;;                                                                      jpachecoxyz.github.io
;;                                 Loading time : %s
;;                                 Packages     : %s
;;
"
                    (emacs-init-time)
                    (number-to-string (length package-activated-list))))
                    (goto-char (point-max)))
  (message (emacs-init-time)))


;; (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
;;   (when (file-exists-p private-file)
;;     (load private-file)))

;;; AUTH-SOURCE
(use-package auth-source
  :ensure nil
  :defer t
  :config
  (setq auth-sources
        (list (expand-file-name ".authinfo.gpg" user-emacs-directory)))
  (setq user-full-name "Javier Pacheco"
        user-mail-address "jpacheco@cock.li")

  ;; Use `pass` as an auth-source
  (when (file-exists-p "~/.password-store")
    (auth-source-pass-enable)))


;;; CONF
(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\..*\\'" "\\.env\\'")
  :init
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))


;;; COMPILATION
(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  ;; Not ideal, but I do not want this poluting the mode-line
  (defun emacs-solo/ignore-compilation-status (&rest _)
    (setq compilation-in-progress nil))
  (advice-add 'compilation-start :after #'emacs-solo/ignore-compilation-status)

  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))


;;; WINDOW
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
     ("\\*container\\*"
      (display-buffer-in-side-window)
      (window-width . 120)
      (side . left)
      (slot . -1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(Ibuffer\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*\\(grep\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     ("\\*\\(M3U Playlist\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 3))

     ("\\*Agenda Commands\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 6)
      (window-width 1.0)
      (dedicated . t))

     ("\\*Org Select\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 6)
      (window-width 1.0)
      (dedicated . t))

     )))


;;; TAB-BAR
(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-min '(10 4))
  (tab-bar-auto-width-max '(50 5))
  :init
  ;; HACK this is an override of the internal function so it
  ;;      shows only the hint number with some decoration.
  (defun tab-bar-tab-name-format-hints (name _tab i)
    "Show absolute numbers on tabs in the tab bar before the tab name.
It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints (concat (format " »%d«" i) "") name)))


;;; RCIRC
(use-package rcirc
  :ensure nil
  :custom
  (rcirc-debug t)
  (rcirc-default-nick "Lionyx")
  (rcirc-default-user-name "Lionyx")
  (rcirc-default-full-name "Lionyx")
  (rcirc-server-alist `(("irc.libera.chat"
                         :channels ("#emacs" "#systemcrafters")
                         :port 6697
                         :encryption tls)))
  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 100)
  (rcirc-track-ignore-server-buffer-flag t)
  :config
  (setopt rcirc-authinfo
          `(("irc.libera.chat" certfp
             ,(expand-file-name "cert.pem" user-emacs-directory)
             ,(expand-file-name "cert.pem" user-emacs-directory)))))


;;; ERC
(use-package erc
  :ensure nil
  :defer t
  :custom
  (erc-join-buffer 'window)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-timestamp-format "[%H:%M]")
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs" "#systemcrafters")))
  :init
  (with-eval-after-load 'erc
    (add-to-list 'erc-modules 'sasl))

  (setopt erc-sasl-mechanism 'external)

  (defun erc-liberachat ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
             :port 6697
             :user "Lionyx"
             :password ""
             :client-certificate
             (list
              (expand-file-name "cert.pem" user-emacs-directory)
              (expand-file-name "cert.pem" user-emacs-directory)))))



;;; DIRED
(use-package dired
  :after evil-collection
  :ensure nil
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  (:map dired-mode-map
        ("-" . dired-up-directory))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  ;; (dired-omit-files "^\\.")                                ; with dired-omit-mode (C-x M-o)
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-open-file)
  :init
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) ;; Turning this ON also sets the C-x M-o binding.

  (defun emacs-solo/dired-rsync-copy (dest)
    "Copy marked files in Dired to DEST using rsync async, with real-time processing of output."
    (interactive
     (list (expand-file-name (read-file-name "rsync to: "
                                             (dired-dwim-target-directory)))))
    (let* ((files (dired-get-marked-files nil current-prefix-arg))
           (dest-original dest)
           (dest-rsync
            (if (file-remote-p dest)
                (let ((vec (tramp-dissect-file-name dest)))
                  (concat (tramp-file-name-user vec)
                          "@"
                          (tramp-file-name-host vec)
                          ":"
                          (tramp-file-name-localname vec)))
              dest))
           (files-rsync
            (mapcar
             (lambda (f)
               (if (file-remote-p f)
                   (let ((vec (tramp-dissect-file-name f)))
                     (concat (tramp-file-name-user vec)
                             "@"
                             (tramp-file-name-host vec)
                             ":"
                             (tramp-file-name-localname vec)))
                 f))
             files))
           (command (append '("rsync" "-hPur") files-rsync (list dest-rsync)))
           (buffer (get-buffer-create "*rsync*")))

      (message "[rsync] original dest: %s" dest-original)
      (message "[rsync] converted dest: %s" dest-rsync)
      (message "[rsync] source files: %s" files-rsync)
      (message "[rsync] command: %s" (string-join command " "))

      (with-current-buffer buffer
        (erase-buffer)
        (insert "Running rsync...\n"))

      (defun rsync-process-filter (proc string)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert string)
          (goto-char (point-max))
          (while (re-search-backward "\r" nil t)
            (replace-match "\n" nil nil))))

      (make-process
       :name "dired-rsync"
       :buffer buffer
       :command command
       :filter 'rsync-process-filter
       :sentinel
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert "\n* rsync done *\n"))
           (dired-revert)))
       :stderr buffer)

      (display-buffer buffer)
      (message "rsync started...")))


  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
                 (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 30)
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t)
                                   (mode-line-format . (" "
                                                        "%b"))))))
      (with-current-buffer dir
        (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
            (rename-buffer "*Dired-Side*")
            )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (eval-after-load 'dired
    '(progn
       (define-key dired-mode-map (kbd "C-<return>") 'emacs-solo/window-dired-open-directory))))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;;; WDIRED
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


;;; ESHELL
(use-package eshell
  :ensure nil
  :bind
  (("C-c e" . eshell))
  :defer t
  :config
  (setopt eshell-banner-message
          (concat
           (propertize " ✨ Welcome to the Emacs Solo Shell ✨\n\n" 'face '(:weight bold :foreground "#f9e2af"))
           (propertize " C-c t" 'face '(:foreground "#89b4fa" :weight bold)) " - toggles between prompts\n"
           (propertize " C-c l" 'face '(:foreground "#89b4fa" :weight bold)) " - searches history\n"
           (propertize " C-l  " 'face '(:foreground "#89b4fa" :weight bold)) " - clears scrolling\n\n"))


  ;; DISABLE SCROLLING CONSERVATIVELY ON ESHELL
  ;;
  (defun emacs-solo/reset-scrolling-vars-for-term ()
    "Locally reset scrolling behavior in term-like buffers."
    (setq-local scroll-conservatively 0)
    (setq-local scroll-margin 0))
  (add-hook 'term-mode-hook #'emacs-solo/reset-scrolling-vars-for-term)
  (add-hook 'eshell-mode-hook #'emacs-solo/reset-scrolling-vars-for-term)


  ;; MAKES C-c l GIVE AN ICOMPLETE LIKE SEARCH TO HISTORY COMMANDS
  ;;
  (defun emacs-solo/eshell-pick-history ()
    "Show Eshell history combining memory and file persistence."
    (interactive)
    ;; Write current session's history to file so it's always fresh
    (when (bound-and-true-p eshell-history-ring)
      (eshell-write-history))
    ;; Then read the history from file
    (let* ((history-file (expand-file-name "eshell/history" user-emacs-directory))
           (history-entries (when (file-exists-p history-file)
                              (with-temp-buffer
                                (insert-file-contents history-file)
                                (split-string (buffer-string) "\n" t))))
           (selection (completing-read "Eshell History: " history-entries)))
      (when selection
        (insert selection))))


  ;; GIVES SYNTAX HIGHLIGHTING TO CAT
  ;;
  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
  Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


  ;; LOCAL ESHELL BINDINGS
  ;;
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") #'emacs-solo/eshell-pick-history)
              (local-set-key (kbd "C-c t") #'emacs-solo/toggle-eshell-prompt)
              (local-set-key (kbd "C-l")
                             (lambda ()
                               (interactive)
                               (eshell/clear 1)))))

  (bind-keys*
  ("C-<backspace>" . (lambda ()
                      (interactive)
                      (when (eq major-mode 'eshell-mode)
                          (goto-char (point-max))
                          (insert "clear 1")
                          (eshell-send-input)))))

  ;; CUSTOM ESHELL PROMPT
  ;;
  (require 'vc)
  (require 'vc-git)

  (defvar emacs-solo/eshell-full-prompt t
    "When non-nil, show the full Eshell prompt. When nil, show minimal prompt.")

  (defvar emacs-solo/eshell-lambda-symbol "𝛌  "
    "Symbol used for the minimal Eshell prompt.")

  (defun emacs-solo/toggle-eshell-prompt ()
    "Toggle between full and minimal Eshell prompt."
    (interactive)
    (setq emacs-solo/eshell-full-prompt (not emacs-solo/eshell-full-prompt))
    (message "Eshell prompt: %s"
             (if emacs-solo/eshell-full-prompt "full" "minimal"))
    (when (derived-mode-p 'eshell-mode)
      (eshell-reset)))

  (setopt eshell-prompt-function
          (lambda ()
            (if emacs-solo/eshell-full-prompt
                ;; Full-blown prompt
                (concat
                 (propertize "" 'face `(:foreground "#212234"))

                 (propertize
                  (if (> eshell-last-command-status 0) " 🔴 " " 🟢 ")
                  'face `(:background "#212234"))

                 (propertize (concat (number-to-string eshell-last-command-status) " ")
                             'face `(:background "#212234"))

                 (propertize "" 'face `(:foreground "#212234" :background "#45475A"))

                 (propertize
                  (let ((remote-user (file-remote-p default-directory 'user))
                        (is-remote (file-remote-p default-directory)))
                    (concat
                     (if is-remote "👽 " "🧙 ")
                     (or remote-user (user-login-name))
                     " "))
                  'face `(:foreground "#89b4fa" :background "#45475A"))

                 (propertize "" 'face `(:foreground "#45475A" :background "#212234"))

                 (let ((remote-host (file-remote-p default-directory 'host))
                       (is-remote (file-remote-p default-directory)))
                   (propertize
                    (concat (if is-remote " 🌐 " " 💻 ")
                            (or remote-host (system-name))
                            " ")
                    'face `(:background "#212234" :foreground "#b4befe")))

                 (propertize "" 'face `(:foreground "#212234" :background "#45475A"))

                 (propertize
                  (concat " 🕒 " (format-time-string "%H:%M:%S" (current-time)) " ")
                  'face `(:foreground "#89b4fa" :background "#45475A"))

                 (propertize "" 'face `(:foreground "#45475A" :background "#212234"))

                 (propertize
                  (concat " 📁 " (if (>= (length (eshell/pwd)) 40)
                                     (concat "…" (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                                   (abbreviate-file-name (eshell/pwd))) " ")
                  'face `(:background "#212234" :foreground "#A6E3A1"))

                 (propertize "\n" 'face `(:foreground "#212234"))

                 (when (and (fboundp 'vc-git-root) (vc-git-root default-directory))
                   (concat
                    (propertize "" 'face `(:foreground "#212234"))
                    (propertize
                     (concat
                      " 🌿 " (car (vc-git-branches))
                      (let* ((branch (car (vc-git-branches)))
                             (behind (string-to-number
                                      (shell-command-to-string
                                       (format "git rev-list --count origin/%s..HEAD" branch))))
                             (ahead (string-to-number
                                     (shell-command-to-string
                                      (format "git rev-list --count HEAD..origin/%s" branch)))))
                        (concat
                         (when (> ahead 0) (format " ⬇️%d" ahead))

                         (when (> behind 0) (format " ⬆️%d" behind))

                         (when (and (> ahead 0) (> behind 0)) "  🔀")))

                      (let ((modified (length (split-string
                                               (shell-command-to-string "git ls-files --modified")
                                               "\n" t)))
                            (untracked (length (split-string
                                                (shell-command-to-string
                                                 "git ls-files --others --exclude-standard")
                                                "\n" t)))
                            (conflicts (length (split-string
                                                (shell-command-to-string
                                                 "git diff --name-only --diff-filter=U")
                                                "\n" t))))
                        (concat
                         (if (> modified 0) (format " ✏️%d" modified))

                         (if (> untracked 0) (format " ✨%d" untracked))

                         (if (> conflicts 0) (format " ⚔️%d" conflicts))))
                      " ")
                     'face `(:background "#212234" :foreground "#F9E2AF"))

                    (propertize "\n" 'face `(:foreground "#212234"))))

                 (propertize emacs-solo/eshell-lambda-symbol 'face font-lock-keyword-face))

              ;; Minimal prompt
              (propertize emacs-solo/eshell-lambda-symbol 'face font-lock-keyword-face))))

  (setq eshell-prompt-regexp emacs-solo/eshell-lambda-symbol)


  ;; SET TERM ENV SO MOST PROGRAMS WON'T COMPLAIN
  ;;
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))


  ;; LIST OF VISUAL COMMANDS TO RUN IN A SEPARATED ANSI-TERM
  ;;
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-subcommands '("jj" "resolve"))
    (add-to-list 'eshell-visual-subcommands '("jj" "squash")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m" "psql"
          "lazygit" "lazydocker" "ncmpcpp" "newsbeuter" "nethack" "mutt" "neomutt" "tmux"
          "docker" "podman" "jqp")))


;;; ISEARCH
(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?")

  (defun isearch-copy-selected-word ()
    "Copy the current `isearch` selection to the kill ring."
    (interactive)
    (when isearch-other-end
      (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
        (kill-new selection)
        (isearch-exit))))

  ;; Bind `M-w` in isearch to copy the selected word, so M-s M-. M-w
  ;; does a great job of 'copying the current word under cursor'.
  (define-key isearch-mode-map (kbd "M-w") 'isearch-copy-selected-word))


;;; VC
(use-package vc
  :ensure nil
  :defer t
  :config
  (setopt
   vc-git-diff-switches '("--patch-with-stat" "--histogram")  ;; add stats to `git diff'
   vc-git-log-switches '("--stat")                            ;; add stats to `git log'
   vc-git-log-edit-summary-target-len 50
   vc-git-log-edit-summary-max-len 70
   vc-git-print-log-follow t
   vc-git-revision-complete-only-branches nil
   vc-annotate-display-mode 'scale
   add-log-keep-changes-together t
   vc-make-backup-files nil)                                  ;; Do not backup version controlled files

  (with-eval-after-load 'vc-annotate
    (setopt vc-annotate-color-map
            '((20 . "#c3e88d")
              (40 . "#89DDFF")
              (60 . "#82aaff")
              (80 . "#676E95")
              (100 . "#c792ea")
              (120 . "#f78c6c")
              (140 . "#79a8ff")
              (160 . "#f5e0dc")
              (180 . "#a6e3a1")
              (200 . "#94e2d5")
              (220 . "#89dceb")
              (240 . "#74c7ec")
              (260 . "#82aaff")
              (280 . "#b4befe")
              (300 . "#b5b0ff")
              (320 . "#8c9eff")
              (340 . "#6a81ff")
              (360 . "#5c6bd7"))))

  ;; This one is for editing commit messages
  (require 'log-edit)
  (setopt log-edit-confirm 'changed
          log-edit-keep-buffer nil
          log-edit-require-final-newline t
          log-edit-setup-add-author nil)

  ;; Removes the bottom window with modified files list
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (with-eval-after-load 'vc-dir
    ;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
    ;; reset, and r run git reset and checkout from head.
    (defun emacs-solo/vc-git-command (verb fn)
      "Execute a Git command with VERB as action description and FN as operation on files."
      (let* ((fileset (vc-deduce-fileset t)) ;; Deduce fileset
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message "%s %d file(s)." verb (length files)))
          (message "Not in a VC Git buffer."))))

    (defun emacs-solo/vc-git-add (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Staged" 'vc-git-register))

    (defun emacs-solo/vc-git-reset (&optional revision vc-fileset comment)
      (interactive "P")
      (emacs-solo/vc-git-command "Unstaged"
                                 (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))


    ;; Bind S and U in vc-dir-mode-map
    (define-key vc-dir-mode-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-dir-mode-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind S and U in vc-prefix-map for general VC usage
    (define-key vc-prefix-map (kbd "S") #'emacs-solo/vc-git-add)
    (define-key vc-prefix-map (kbd "U") #'emacs-solo/vc-git-reset)

    ;; Bind g to hide up to date files after refreshing in vc-dir
    (define-key vc-dir-mode-map (kbd "g")
                (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))


    (defun emacs-solo/vc-git-visualize-status ()
      "Show the Git status of files in the `vc-log` buffer."
      (interactive)
      (let* ((fileset (vc-deduce-fileset t))
             (backend (car fileset))
             (files (nth 1 fileset)))
        (if (eq backend 'Git)
            (let ((output-buffer "*Git Status*"))
              (with-current-buffer (get-buffer-create output-buffer)
                (read-only-mode -1)
                (erase-buffer)
                ;; Capture the raw output including colors using 'git status --color=auto'
                (call-process "git" nil output-buffer nil "status" "-v")
                (pop-to-buffer output-buffer)))
          (message "Not in a VC Git buffer."))))

    (define-key vc-dir-mode-map (kbd "V") #'emacs-solo/vc-git-visualize-status)
    (define-key vc-prefix-map (kbd "V") #'emacs-solo/vc-git-visualize-status))

  (defun emacs-solo/vc-git-reflog ()
    "Show git reflog in a new buffer with ANSI colors and custom keybindings."
    (interactive)
    (let* ((root (vc-root-dir)) ;; Capture VC root before creating buffer
           (buffer (get-buffer-create "*vc-git-reflog*")))
      (with-current-buffer buffer
        (setq-local vc-git-reflog-root root) ;; Store VC root as a buffer-local variable
        (let ((inhibit-read-only t))
          (erase-buffer)
          (vc-git-command buffer nil nil
                          "reflog"
                          "--color=always"
                          "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
          (goto-char (point-min))
          (ansi-color-apply-on-region (point-min) (point-max)))

        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "/") #'isearch-forward)
          (define-key map (kbd "p") #'previous-line)
          (define-key map (kbd "n") #'next-line)
          (define-key map (kbd "q") #'kill-buffer-and-window)

          (use-local-map map))

        (setq buffer-read-only t)
        (setq mode-name "Git-Reflog")
        (setq major-mode 'special-mode))
      (pop-to-buffer buffer)))
  (global-set-key (kbd "C-x v R") 'emacs-solo/vc-git-reflog)


  (defun emacs-solo/vc-pull-merge-current-branch ()
    "Pull the latest change from origin for the current branch and display output in a buffer."
    (interactive)
    (let* ((branch (vc-git--symbolic-ref "HEAD"))
           (buffer (get-buffer-create "*Git Pull Output*"))
           (command (format "git pull origin %s" branch)))
      (if branch
          (progn
            (with-current-buffer buffer
              (erase-buffer)
              (insert (format "$ %s\n\n" command))
              (call-process-shell-command command nil buffer t))
            (display-buffer buffer))
        (message "Could not determine current branch."))))


  (defun emacs-solo/vc-browse-remote (&optional current-line)
    "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
    (interactive "P")
    (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
           (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
           (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
           (line (line-number-at-pos)))
      (message "Opening remote on browser: %s" remote-url)
      (if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
          (let ((host (match-string 1 remote-url))
                (path (match-string 2 remote-url)))
            ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
            (when (string-prefix-p "git@" host)
              (setq host (replace-regexp-in-string "^git@" "" host)))
            ;; Construct the appropriate URL based on CURRENT-LINE
            (browse-url
             (if current-line
                 (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
               (format "https://%s/%s" host path))))
        (message "Could not determine repository URL"))))
  (global-set-key (kbd "C-x v B") 'emacs-solo/vc-browse-remote)
  (global-set-key (kbd "C-x v o")
                  '(lambda () (interactive) (emacs-solo/vc-browse-remote 1)))


  (defun emacs-solo/vc-diff-on-current-hunk ()
    "Show the diff for the current file and jump to the hunk containing the current line."
    (interactive)
    (let ((current-line (line-number-at-pos)))
      (message "Current line in file: %d" current-line)
      (vc-diff) ; Generate the diff buffer
      (with-current-buffer "*vc-diff*"
        (goto-char (point-min))
        (let ((found-hunk nil))
          (while (and (not found-hunk)
                      (re-search-forward "^@@ -\\([0-9]+\\), *[0-9]+ \\+\\([0-9]+\\), *\\([0-9]+\\) @@" nil t))
            (let* ((start-line (string-to-number (match-string 2)))
                   (line-count (string-to-number (match-string 3)))
                   (end-line (+ start-line line-count)))
              (message "Found hunk: %d to %d" start-line end-line)
              (when (and (>= current-line start-line)
                         (<= current-line end-line))
                (message "Current line %d is within hunk range %d to %d" current-line start-line end-line)
                (setq found-hunk t)
                (goto-char (match-beginning 0))))) ; Jump to the beginning of the hunk
          (unless found-hunk
            (message "Current line %d is not within any hunk range." current-line)
            (goto-char (point-min)))))))
  (global-set-key (kbd "C-x v =") 'emacs-solo/vc-diff-on-current-hunk))

;;; SMERGE
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

;;; DIFF
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-font-lock-prettify nil))

;;; EDIFF
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;; ELDOC
(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

;;; EGLOT
(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-prefer-plaintext t)
  (jsonrpc-event-hook nil)
  (eglot-code-action-indications nil) ;; EMACS-31 -- annoying as hell
  :init
  (fset #'jsonrpc--log-event #'ignore)

  (setq-default eglot-workspace-configuration (quote
                                               (:gopls (:hints (:parameterNames t)))))

  (defun emacs-solo/eglot-setup ()
    "Setup eglot mode with specific exclusions."
    (unless (eq major-mode 'emacs-lisp-mode)
      (eglot-ensure)))

  (add-hook 'prog-mode-hook #'emacs-solo/eglot-setup)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

  :bind (:map
         eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

;;; FLYMAKE
;; (use-package flymake
;;   :ensure nil
;;   :defer t
;;   :hook (prog-mode-hook . flymake-mode)
;;   :bind (:map flymake-mode-map
;;               ("M-8" . flymake-goto-next-error)
;;               ("M-7" . flymake-goto-prev-error)
;;               ("C-c ! n" . flymake-goto-next-error)
;;               ("C-c ! p" . flymake-goto-prev-error)
;;               ("C-c ! l" . flymake-show-buffer-diagnostics)
;;               ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
;;   :custom
;;   (flymake-show-diagnostics-at-end-of-line nil)
;;   ;; (flymake-show-diagnostics-at-end-of-line 'short)
;;   (flymake-indicator-type 'margins)
;;   (flymake-margin-indicators-string
;;    `((error "!" compilation-error)      ;; Alternatives: », E, W, i, !, ?)
;;      (warning "?" compilation-warning)
;;      (note "i" compilation-info)))
;;   :config
;;   ;; Define the toggle function
;;   (defun toggle-flymake-diagnostics-at-eol ()
;;     "Toggle the display of Flymake diagnostics at the end of the line
;; and restart Flymake to apply the changes."
;;     (interactive)
;;     (setq flymake-show-diagnostics-at-end-of-line
;;           (not flymake-show-diagnostics-at-end-of-line))
;;     (flymake-mode -1) ;; Disable Flymake
;;     (flymake-mode 1)  ;; Re-enable Flymake
;;     (message "Flymake diagnostics at end of line: %s"
;;              (if flymake-show-diagnostics-at-end-of-line
;;                  "Enabled" "Disabled"))))


;;; WHITESPACE
(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save-hook . whitespace-cleanup)
  ;; if we wanna remove this hook at any time, eval:
  ;; (remove-hook 'before-save-hook #'whitespace-cleanup)
  )


;;; GNUS
(use-package gnus
  :ensure nil
  :defer t
  :custom
  (gnus-init-file (concat user-emacs-directory ".gnus.el"))
  (gnus-startup-file (concat user-emacs-directory ".newsrc"))
  (gnus-init-file (concat user-emacs-directory ".newsrc.eld"))
  (gnus-activate-level 3)
  (gnus-message-archive-group nil)
  (gnus-check-new-newsgroups nil)
  (gnus-check-bogus-newsgroups nil)
  (gnus-show-threads nil)
  (gnus-use-cross-reference nil)
  (gnus-nov-is-evil nil)
  (gnus-group-line-format "%1M%5y  : %(%-50,50G%)\12")
  (gnus-logo-colors '("#2fdbde" "#c0c0c0"))
  (gnus-permanently-visible-groups ".*")
  (gnus-summary-insert-entire-threads t)
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-number
     gnus-thread-sort-by-subject
     (not gnus-thread-sort-by-total-score)
     gnus-thread-sort-by-most-recent-date))
  (gnus-summary-line-format "%U%R%z: %[%d%] %4{ %-34,34n%} %3{  %}%(%1{%B%}%s%)\12")
  (gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M")))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-sum--tree-indent " ")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-false-root "○ ")
  (gnus-sum-thread-tree-single-indent "◎ ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│)")
  (gnus-select-method '(nnnil nil))
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-secondary-select-methods
   '((nntp "news.gwene.org"))))


;;; MAN
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'


;;; MINIBUFFER
(use-package minibuffer
  :ensure nil
  :custom
  (completion-styles '(partial-completion flex initials))
  (completion-ignore-case t)
  (completion-show-help t)
  ;; (completion-auto-select t) ;; NOTE: only turn this on if not using icomplete, can also be 'second-tab
  (completions-max-height 20)
  (completions-format 'one-column)
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :config
  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq truncate-lines t)))

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))


;;; NEWSTICKER
(use-package newsticker
  :ensure nil
  :defer t
  :custom
  (newsticker-treeview-treewindow-width 40)
  :hook
  (newsticker-treeview-mode-hook . (lambda ()
                                     (define-key newsticker-treeview-mode-map
                                                 (kbd "V")
                                                 'emacs-solo/newsticker-play-yt-video-from-buffer)
                                     (define-key newsticker-treeview-list-mode-map
                                                 (kbd "V")
                                                 'emacs-solo/newsticker-play-yt-video-from-buffer)
                                     (define-key newsticker-treeview-item-mode-map
                                                 (kbd "V")
                                                 'emacs-solo/newsticker-play-yt-video-from-buffer)))
  :init
  (defun emacs-solo/newsticker-play-yt-video-from-buffer ()
    "Focus the window showing '*Newsticker Item*' and play the video."
    (interactive)
    (let ((window (get-buffer-window "*Newsticker Item*" t)))
      (if window
          (progn
            (select-window window)
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^\\* videoId: \\(\\w+\\)" nil t)
                (let ((video-id (match-string 1)))
                  (start-process "mpv-video" nil "mpv" (format "https://www.youtube.com/watch?v=%s" video-id))
                  (message "Playing with mpv: %s" video-id))))))
      (message "No window showing *Newsticker Item* buffer."))))


;;; ELEC_PAIR
(use-package elec-pair
  :ensure nil
  :defer
  :hook (after-init-hook . electric-pair-mode))

;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
               (setq-local electric-pair-inhibit-predicate
                       `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;;; PAREN
(use-package paren
  :ensure nil
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t)) ;; show matches within window splits

;;; PROCED
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;;; ORG
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq
   ;; Start collapsed for speed
   org-startup-folded t

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀─────── now ──")

  ;; Ellipsis styling
  (setq org-ellipsis " ⤵")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))


;;; TIME
(use-package time
  :ensure nil
  ;; :hook (after-init-hook . display-time-mode) ;; If we'd like to see it on the mode-line
  :custom
  (world-clock-time-format "%A %d %B %r %Z")
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-mail-string "")
  (zoneinfo-style-world-list                ; use `M-x worldclock RET' to see it
   '(("America/Los_Angeles" "Los Angeles")
     ("America/Vancouver" "Vancouver")
     ("Canada/Pacific" "Canada/Pacific")
     ("America/Chicago" "Chicago")
     ("America/Toronto" "Toronto")
     ("America/New_York" "New York")
     ("Canada/Atlantic" "Canada/Atlantic")
     ("Brazil/East" "Brasília")
     ("America/Sao_Paulo" "São Paulo")
     ("UTC" "UTC")
     ("Europe/Lisbon" "Lisbon")
     ("Europe/Brussels" "Brussels")
     ("Europe/Athens" "Athens")
     ("Asia/Riyadh" "Riyadh")
     ("Asia/Tehran" "Tehran")
     ("Asia/Tbilisi" "Tbilisi")
     ("Asia/Yekaterinburg" "Yekaterinburg")
     ("Asia/Kolkata" "Kolkata")
     ("Asia/Singapore" "Singapore")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Seoul" "Seoul")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Vladivostok" "Vladivostok")
     ("Australia/Brisbane" "Brisbane")
     ("Australia/Sydney" "Sydney")
     ("Pacific/Auckland" "Auckland"))))


;;; UNIQUIFY
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))


;;; WHICH-KEY
(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode)
  :config
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "… ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)

  ;; Inspired by: https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
  ;;
  ;; By default, Which-key doesn't give much help for prefix-keys. It
  ;; either shows the generic description, "+prefix", or the name of a
  ;; prefix-command, which usually isn't as descriptive as we'd like.
  ;;
  ;; Here are some descriptions for the default bindings in `global-map'
  ;; and `org-mode-map'.
  (which-key-add-key-based-replacements
    "<f1> 4" "help-other-win"
    "<f1>" "help"
    "<f2>" "2column"
    "C-c" "mode-and-user"
    "C-c !" "flymake"
    "C-c g" "git-gutter"
    "C-h 4" "help-other-win"
    "C-h" "help"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x 6" "2-column"
    "C-x 8" "insert-special"
    "C-x 8 ^" "superscript (⁰, ¹, ², …)"
    "C-x 8 _" "subscript (₀, ₁, ₂, …)"
    "C-x 8 a" "arrows & æ (←, →, ↔, æ)"
    "C-x 8 e" "emojis (🫎, 🇧🇷, 🇮🇹, …)"
    "C-x 8 *" "common symbols ( , ¡, €, …)"
    "C-x 8 =" "macron (Ā, Ē, Ḡ, …)"
    "C-x 8 N" "macron (№)"
    "C-x 8 O" "macron (œ)"
    "C-x 8 ~" "tilde (~, ã, …)"
    "C-x 8 /" "stroke (÷, ≠, ø, …)"
    "C-x 8 ." "dot (·, ż)"
    "C-x 8 ," "cedilla (¸, ç, ą, …)"
    "C-x 8 '" "acute (á, é, í, …)"
    "C-x 8 `" "grave (à, è, ì, …)"
    "C-x 8 \"" "quotation/dieresis (\", ë, ß, …)"
    "C-x 8 1" "†, 1/…"
    "C-x 8 2" "‡"
    "C-x 8 3" "3/…"
    "C-x C-k C-q" "kmacro-counters"
    "C-x C-k C-r a" "kmacro-add"
    "C-x C-k C-r" "kmacro-register"
    "C-x C-k" "keyboard-macros"
    "C-x RET" "encoding/input"
    "C-x a i" "abbrevs-inverse-add"
    "C-x a" "abbrevs"
    "C-x n" "narrowing"
    "C-x p" "projects"
    "C-x r" "reg/rect/bkmks"
    "C-x t ^" "tab-bar-detach"
    "C-x t" "tab-bar"
    "C-x v M" "vc-mergebase"
    "C-x v b" "vc-branch"
    "C-x v" "version-control"
    "C-x w ^" "window-detach"
    "C-x w" "window-extras"
    "C-x x" "buffer-extras"
    "C-x" "extra-commands"
    "M-g" "goto-map"
    "M-s h" "search-highlight"
    "M-s" "search-map")

  ;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
  ;; a prefix-key. If you know of other built-in packages that have
  ;; this behavior, please let me know, so I can add them.
  (with-eval-after-load 'page-ext
    (which-key-add-key-based-replacements
      "C-x C-p" "page-extras"))

  ;; Org-mode provides some additional prefix-keys in `org-mode-map'.
  (with-eval-after-load 'org
    (which-key-add-keymap-based-replacements org-mode-map
      "C-c \"" "org-plot"
      "C-c C-v" "org-babel"
      "C-c C-x" "org-extra-commands")))


;;; WEBJUMP
(use-package webjump
  :defer t
  :ensure nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
     ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/results?search_query=" ""])
     ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))


;;; THEMES
(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((bg-main "#222222")
     (bg-active bg-main)
     (fg-main "#EEFFFF")
     (fg-active fg-main)
     (fg-mode-line-active "#A6Accd")
     (bg-mode-line-active "#232635")
     (fg-mode-line-inactive "#676E95")
     (bg-mode-line-inactive "#444444")
     ;; (border-mode-line-active "#676E95")
     ;; (border-mode-line-inactive bg-dim)
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (bg-tab-bar      "#242837")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#242837")
     (fg-prompt "#c792ea")
     (bg-prompt unspecified)
     (bg-hover-secondary "#676E95")
     (bg-completion "#2f447f")
     (fg-completion white)
     (bg-region "#3C435E")
     (fg-region white)

     (fg-line-number-active fg-main)
     (fg-line-number-inactive "gray50")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#222222")
     (fringe "#222222")

     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")

     (fg-prose-verbatim "#c3e88d")
     (bg-prose-block-contents "#232635")
     (fg-prose-block-delimiter "#676E95")
     (bg-prose-block-delimiter bg-prose-block-contents)

     (accent-1 "#79a8ff")

     (keyword "#89DDFF")
     (builtin "#82aaff")
     (comment "#676E95")
     (string "#c3e88d")
     (fnname "#82aaff")
     (type "#c792ea")
     (variable "#c792ea")
     (docstring "#8d92af")
     (constant "#f78c6c")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(tab-bar
       ((,c
         :background "#232635"
         :foreground "#A6Accd"
         ;; :box (:line-width 1 :color "#676E95")
         )))
    `(org-block
        ((,c :background "#2f2f2f")))
    `(org-block-begin-line
        ((,c :background "#222222")))
    `(org-block-end-line
        ((,c :background "#222222")))

        `(tab-bar-tab
        ((,c
         ;; :background "#232635"
         ;; :underline t
         ;; :box (:line-width 1 :color "#676E95")
         )))
     `(tab-bar-tab-inactive
       ((,c
         ;; :background "#232635"
         ;; :box (:line-width 1 :color "#676E95")
         )))))
  :init
  (load-theme 'modus-vivendi-tinted t))


;;; -------------------- NON TREESITTER AREA
;;; SASS-MODE
(use-package scss-mode
  :mode "\\.sass\\'"
  :hook
  ((scss-mode-hook . (lambda ()
                       (setq indent-tabs-mode nil))))
  :defer t)


;;; -------------------- TREESITTER AREA
;;; RUBY-TS-MODE
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :custom
  (add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src"))
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))


;;; JS-TS-MODE
(use-package js-ts-mode
  :ensure js ;; I care about js-base-mode but it is locked behind the feature "js"
  :mode "\\.jsx?\\'"
  :defer t
  :hook
  ((js-ts-mode-hook . (lambda ()
                        (setq indent-tabs-mode nil))))
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (add-to-list 'treesit-language-source-alist '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")))


;;; TYPESCRIPT-TS-MODE
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :defer t
  :hook
  ((typescript-ts-mode-hook . (lambda ()
                                (setq indent-tabs-mode nil))))
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))


;;; TYPESCRIPT-TS-MODE
(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :defer t
  :hook
  ((tsx-ts-mode-hook . (lambda ()
                         (setq indent-tabs-mode nil))))
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (unbind-key "M-." typescript-ts-base-mode-map))


;;; RUST-TS-MODE
(use-package rust-ts-mode
  :ensure rust-ts-mode
  :mode "\\.rs\\'"
  :defer t
  :custom
  (rust-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")))


;;; TOML-TS-MODE
(use-package toml-ts-mode
  :ensure toml-ts-mode
  :mode "\\.toml\\'"
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))


;;; MARKDOWN-TS-MODE - EMACS-31
;;  As I first proposed here:
;;  https://lists.gnu.org/archive/html/emacs-devel/2025-02/msg00810.html
(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :defer t
  :config
  ;; (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))


;;; YAML-TS-MODE
(use-package yaml-ts-mode
  :ensure yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")))


;;; DOCKERFILE-TS-MODE
(use-package dockerfile-ts-mode
  :ensure dockerfile-ts-mode
  :mode "\\Dockerfile.*\\'"
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))


;;; GO-TS-MODE
(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  ((go-ts-mode-hook . (lambda ()
                        (setq indent-tabs-mode t)  ; Use tabs, go likes tabs, go figure
                        (setq tab-width 4)         ; Tabs *display* as 4 spaces
                        (setq-local go-ts-mode-indent-offset tab-width))))
  :defer t)

;;; ------------------- EMACS-SOLO CUSTOMS
;;; EMACS-SOLO-HOOKS
;;
(use-package emacs-solo-hooks
  :ensure nil
  :no-require t
  :defer t
  :init

  (defun emacs-solo/prefer-spaces ()
    "Disable indent-tabs-mode to prefer spaces over tabs."
    (interactive)
    (setq indent-tabs-mode nil))

  ;; Only override where necessary
  (add-hook 'emacs-lisp-mode-hook #'emacs-solo/prefer-spaces))


;;; EMACS-SOLO-MOVEMENTS
;;
;;  Functions to better move around text and Emacs
;;
(use-package emacs-solo-movements
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rename-buffer-and-move-to-new-window ()
    "Promotes a side window buffer to a new regular window."
    (interactive)
    (let ((temp-name (make-temp-name "temp-buffer-")))
      (rename-buffer temp-name t)
      (delete-window)
      (split-window-right)
      (switch-to-buffer temp-name)))

  (global-set-key (kbd "C-x x x") 'emacs-solo/rename-buffer-and-move-to-new-window)


  (defun emacs-solo-movements/scroll-down-centralize ()
    (interactive)
    (scroll-up-command)
    (recenter))

  (defun emacs-solo-movements/scroll-up-centralize ()
    (interactive)
    (scroll-down-command)
    (unless (= (window-start) (point-min))
      (recenter))
    (when (= (window-start) (point-min))
      (let ((midpoint (/ (window-height) 2)))
        (goto-char (window-start))
        (forward-line midpoint)
        (recenter midpoint))))

  (global-set-key (kbd "C-v") #'emacs-solo-movements/scroll-down-centralize)
  (global-set-key (kbd "M-v") #'emacs-solo-movements/scroll-up-centralize)


  (defun emacs-solo-movements/format-current-file ()
    "Format the current file using biome if biome.json is present; otherwise, use prettier.
Also first tries the local node_modules/.bin and later the global bin."
    (interactive)
    (let* ((file (buffer-file-name))
           (project-root (locate-dominating-file file "node_modules"))
           (biome-config (and project-root (file-exists-p (expand-file-name "biome.json" project-root))))
           (local-biome (and project-root (expand-file-name "node_modules/.bin/biome" project-root)))
           (global-biome (executable-find "biome"))
           (local-prettier (and project-root (expand-file-name "node_modules/.bin/prettier" project-root)))
           (global-prettier (executable-find "prettier"))
           (formatter nil)
           (source nil)
           (command nil)
           (start-time (float-time))) ;; Capture the start time
      (cond
       ;; Use Biome if biome.json exists
       ((and biome-config local-biome (file-executable-p local-biome))
        (setq formatter local-biome)
        (setq source "biome (local)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))
       ((and biome-config global-biome)
        (setq formatter global-biome)
        (setq source "biome (global)")
        (setq command (format "%s format --write %s" formatter (shell-quote-argument file))))

       ;; Fall back to Prettier if no biome.json
       ((and local-prettier (file-executable-p local-prettier))
        (setq formatter local-prettier)
        (setq source "prettier (local)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file))))
       ((and global-prettier)
        (setq formatter global-prettier)
        (setq source "prettier (global)")
        (setq command (format "%s --write %s" formatter (shell-quote-argument file)))))
      (if command
          (progn
            (save-buffer)
            (shell-command command)
            (revert-buffer t t t)
            (let ((elapsed-time (* 1000 (- (float-time) start-time)))) ;; Calculate elapsed time in ms
              (message "Formatted with %s - %.2f ms" source elapsed-time)))
        (message "No formatter found (biome or prettier)"))))

  (global-set-key (kbd "C-c p") #'emacs-solo-movements/format-current-file)
  (global-set-key (kbd "C-c C-p") #'emacs-solo-movements/format-current-file)


  (defun emacs-solo/transpose-split ()
    "Transpose a horizontal split into a vertical split, or vice versa."
    (interactive)
    (if (> (length (window-list)) 2)
        (user-error "More than two windows present")
      (let* ((this-win (selected-window))
             (other-win (next-window))
             (this-buf (window-buffer this-win))
             (other-buf (window-buffer other-win))
             (this-edges (window-edges this-win))
             (other-edges (window-edges other-win))
             (this-left (car this-edges))
             (other-left (car other-edges))
             (split-horizontally (not (= this-left other-left))))
        (delete-other-windows)
        (if split-horizontally
            (split-window-vertically)
          (split-window-horizontally))
        (set-window-buffer (selected-window) this-buf)
        (set-window-buffer (next-window) other-buf)
        (select-window this-win))))

  (global-set-key (kbd "C-x 4 t") #'emacs-solo/transpose-split))


;;; EMACS-SOLO-TRANSPARENCY
;;
;;  Custom functions to set/unset transparency
;;
(use-package emacs-solo-transparency
  :if emacs-solo-enable-transparency
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/clear-terminal-background-color (&optional frame)
    "Unsets the background color in terminal mode, including line-number face."
    (interactive)
    (or frame (setq frame (selected-frame)))
    (unless (display-graphic-p frame)
      ;; Set the terminal to a transparent version of the background color
      (send-string-to-terminal
       (format "\033]11;[90]%s\033\\"
               (face-attribute 'default :background)))
      (set-face-background 'default "unspecified-bg" frame)
      (set-face-background 'line-number "unspecified-bg" frame)
      (set-face-background 'line-number-current-line "unspecified-bg" frame)))

  (defun emacs-solo/transparency-set (&optional frame)
    "Set frame transparency. If FRAME is nil, applies to all existing frames."
    (interactive)
    (unless (display-graphic-p frame)
      (add-hook 'window-setup-hook 'emacs-solo/clear-terminal-background-color)
      (add-hook 'ef-themes-post-load-hook 'emacs-solo/clear-terminal-background-color))

    (if frame
        (progn
          (when (eq system-type 'darwin)
            (set-frame-parameter frame 'alpha '(90 90)))
          (set-frame-parameter frame 'alpha-background 85))

      ;; Apply to all frames if no frame is passed
      (dolist (frm (frame-list))
        (when (eq system-type 'darwin)
          (set-frame-parameter frm 'alpha '(90 90)))
        (set-frame-parameter frm 'alpha-background 85))))

  (defun emacs-solo/transparency-unset ()
    "Unset frame transparency (Graphical Mode)."
    (interactive)
    (when (eq system-type 'darwin)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background 100)))

  (add-hook 'after-init-hook #'emacs-solo/transparency-set)
  (add-hook 'after-make-frame-functions #'emacs-solo/transparency-set))


;;; EMACS-SOLO-MODE-LINE
;;
;;  Customizations to the mode-line
;;
(use-package emacs-solo-mode-line
  :ensure nil
  :no-require t
  :defer t
  :init
  ;; Shorten big branches names
  (defun emacs-solo/shorten-vc-mode (vc)
    "Shorten VC string to at most 20 characters.
Replacing `Git-' with a branch symbol."
    (let* ((vc (replace-regexp-in-string "^ Git[:-]"
                                         (if (char-displayable-p ?) "  " "Git: ")
                                         vc))) ;; Options:   ᚠ ⎇
      (if (> (length vc) 20)
          (concat (substring vc 0 20)
                  (if (char-displayable-p ?…) "…" "..."))
        vc)))

  ;; Formats mode-line
  (setq-default mode-line-format
                '("%e" "  "
                  ;; (:propertize " " display (raise +0.1)) ;; Top padding
                  ;; (:propertize " " display (raise -0.1)) ;; Bottom padding
                ;; Display Evil state first
                (:eval
                    (let ((tag (upcase
                                (cond
                                ((evil-insert-state-p) "Insert")
                                ((evil-normal-state-p) "Normal")
                                ((evil-visual-state-p) "Visual")
                                ((evil-replace-state-p) "Replace")
                                ((evil-motion-state-p) "Motion")
                                ((evil-emacs-state-p) "Emacs")
                                (t (symbol-name evil-state))))))
                    (propertize (format "[%s] " tag)
                                'face 'font-lock-builtin-face)))
                  ;; (:propertize "𝛌  " face font-lock-keyword-face)

                  ;; (:propertize
                  ;;  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))

                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  mode-line-format-right-align
                  "  "
                  (project-mode-line project-mode-line-format)
                  "  "
                  (vc-mode (:eval (emacs-solo/shorten-vc-mode vc-mode)))
                  "  "
                  ;; mode-line-modes
                  mode-line-misc-info
                  "  ")
                project-mode-line t
                mode-line-buffer-identification '(" %b")
                mode-line-position-column-line-format '(" %l:%c"))

  ;; EMACS-31
  (setq mode-line-collapse-minor-modes
        '(abbrev-mode
          eldoc-mode
          flyspell-mode
          smooth-scroll-mode
          outline-minor-mode
          which-key-mode))

(setq evil-normal-state-tag   (propertize "[Normal]" )
      evil-emacs-state-tag    (propertize "[Emacs]" )
      evil-insert-state-tag   (propertize "[Insert]" )
      evil-motion-state-tag   (propertize "[Motion]" )
      evil-visual-state-tag   (propertize "[Visual]" )
      evil-operator-state-tag (propertize "[Operator]" ))

(setq evil-insert-state-message nil)
(setq evil-visual-state-message nil)
(setq evil-replace-state-message nil)
(setq evil-motion-state-message nil)
(setq hl-line-sticky-flag t)

  (defvar emacs-solo-hidden-minor-modes mode-line-collapse-minor-modes)

  (defun emacs-solo/purge-minor-modes ()
    (interactive)
    (dolist (x emacs-solo-hidden-minor-modes nil)
      (let ((trg (cdr (assoc x minor-mode-alist))))
        (when trg
          (setcar trg "")))))

  (if (< emacs-major-version 31)
      (add-hook 'after-change-major-mode-hook 'emacs-solo/purge-minor-modes)))


;;; EMACS-SOLO-EXEC-PATH-FROM-SHELL
;;
;;  Loads users default shell PATH settings into Emacs. Usefull
;;  when calling Emacs directly from GUI systems.
;;
(use-package emacs-solo-exec-path-from-shell
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as user Shell."
    (interactive)
    (let ((path-from-shell
           (replace-regexp-in-string
            "[ \t\n]*$" "" (shell-command-to-string
                            "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))
      (message ">>> emacs-solo: PATH loaded")))

  (defun emacs-solo/fix-asdf-path ()
    "Ensure asdf shims and active Node.js version's bin directory are first in PATH."
    (interactive)
    (let* ((asdf-shims (expand-file-name "~/.asdf/shims"))
           (node-bin (string-trim (shell-command-to-string "asdf where nodejs 2>/dev/null")))
           (new-paths (list asdf-shims)))

      ;; If Node.js is installed, add its bin path
      (when (file-directory-p node-bin)
        (push (concat node-bin "/bin") new-paths))

      ;; Remove old asdf-related paths from PATH and exec-path
      (setq exec-path (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p)) exec-path))
      (setenv "PATH" (string-join (seq-remove (lambda (p) (string-match-p "/\\.asdf/" p))
                                              (split-string (getenv "PATH") ":"))
                                  ":"))

      ;; Add the new paths to exec-path and PATH
      (dolist (p (reverse new-paths))
        (unless (member p exec-path) (push p exec-path))
        (unless (member p (split-string (getenv "PATH") ":"))
          (setenv "PATH" (concat p ":" (getenv "PATH")))))))

  (add-hook 'find-file-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-mode-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-pre-command-hook #'emacs-solo/fix-asdf-path)
  (add-hook 'eshell-directory-change-hook #'emacs-solo/fix-asdf-path)

  (add-hook 'after-init-hook #'emacs-solo/set-exec-path-from-shell-PATH)
  (add-hook 'after-init-hook #'emacs-solo/fix-asdf-path))


;;; EMACS-SOLO-RAINBOW-DELIMITERS
;;
;;  Colorizes matching delimiters
;;
;;  FIXME: Make it play nice with treesitter modes
;;
(use-package emacs-solo-rainbow-delimiters
  :if emacs-solo-enable-rainbown-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-keyword-face
                    font-lock-type-face
                    font-lock-function-name-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-builtin-face
                    font-lock-string-face
                    )))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))


;;; EMACS-SOLO-PROJECT-SELECT
;;
;;  Interactively finds a project in a Projects folder and sets it
;;  to current `project.el' project.
;;
(use-package emacs-solo-project-select
  :ensure nil
  :no-require t
  :init
  (defvar emacs-solo-default-projects-folder "~/Projects"
    "Default folder to search for projects.")

  (defvar emacs-solo-default-projects-input "**"
    "Default input to use when finding a project.")

  (defun emacs-solo/find-projects-and-switch (&optional directory)
    "Find and switch to a project directory from ~/Projects."
    (interactive)
    (let* ((d (or directory emacs-solo-default-projects-folder))
           ;; (find-command (concat "fd --type d --max-depth 4 . " d))           ; with fd
           (find-command (concat "find " d " -mindepth 1 -maxdepth 4 -type d"))  ; with find
           (project-list (split-string (shell-command-to-string find-command) "\n" t))
           (initial-input emacs-solo-default-projects-input))
      (let ((selected-project
             (completing-read
              "Search project folder: "
              project-list
              nil nil
              initial-input)))
        (when (and selected-project (file-directory-p selected-project))
          (project-switch-project selected-project)))))

  (defun emacs-solo/minibuffer-move-cursor ()
    "Move cursor between `*` characters when minibuffer is populated with `**`."
    (when (string-prefix-p emacs-solo-default-projects-input (minibuffer-contents))
      (goto-char (+ (minibuffer-prompt-end) 1))))

  (add-hook 'minibuffer-setup-hook #'emacs-solo/minibuffer-move-cursor)

  :bind (:map project-prefix-map
              ("P" . emacs-solo/find-projects-and-switch)))


;;; EMACS-SOLO-VIPER-EXTENSIONS
;;
;;  Better VIM (and not VI) bindings for viper-mode
;;
(use-package emacs-solo-viper-extensions
  :ensure nil
  :no-require t
  :defer t
  :after viper
  :init
  (defun viper-operate-inside-delimiters (open close op)
    "Perform OP inside delimiters OPEN and CLOSE (e.g., (), {}, '', or \"\")."
    (save-excursion
      (search-backward (char-to-string open) nil t)
      (forward-char) ;; Move past the opening delimiter
      (let ((start (point)))
        (search-forward (char-to-string close) nil t)
        (backward-char) ;; Move back before the closing delimiter
        (pulse-momentary-highlight-region start (point))
        (funcall op start (point)))))

  ;; FIXME: works for most common cases, misses (  bla bla (bla) |cursor-here| )
  (defun viper-delete-inside-delimiters (open close)
    "Delete text inside delimiters OPEN and CLOSE, saving it to the kill ring."
    (interactive "cEnter opening delimiter: \ncEnter closing delimiter: ")
    (viper-operate-inside-delimiters open close 'kill-region))

  (defun viper-yank-inside-delimiters (open close)
    "Copy text inside delimiters OPEN and CLOSE to the kill ring."
    (interactive "cEnter opening delimiter: \ncEnter closing delimiter: ")
    (viper-operate-inside-delimiters open close 'kill-ring-save))

  (defun viper-delete-line-or-region ()
    "Delete the current line or the selected region in Viper mode.
The deleted text is saved to the kill ring."
    (interactive)
    (if (use-region-p)
        ;; If a region is active, delete it
        (progn
          (pulse-momentary-highlight-region (region-beginning) (region-end))
          (run-at-time 0.1 nil 'kill-region (region-beginning) (region-end)))
      ;; Otherwise, delete the current line including its newline character
      (pulse-momentary-highlight-region (line-beginning-position) (line-beginning-position 2))
      (run-at-time 0.1 nil 'kill-region (line-beginning-position) (line-beginning-position 2))))

  (defun viper-yank-line-or-region ()
    "Yank the current line or the selected region and highlight the region."
    (interactive)
    (if (use-region-p)
        ;; If a region is selected, yank it
        (progn
          (kill-ring-save (region-beginning) (region-end))  ;; Yank the region
          (pulse-momentary-highlight-region (region-beginning) (region-end)))
      ;; Otherwise, yank the current line
      (let ((start (line-beginning-position))
            (end (line-end-position)))
        (kill-ring-save start end)  ;; Yank the current line
        (pulse-momentary-highlight-region start end))))

  (defun viper-visual-select ()
    "Start visual selection from the current position."
    (interactive)
    (set-mark (point)))

  (defun viper-visual-select-line ()
    "Start visual selection from the beginning of the current line."
    (interactive)
    (set-mark (line-beginning-position)))

  (defun viper-delete-inner-word ()
    "Delete the current word under the cursor, handling edge cases."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (kill-region (car bounds) (cdr bounds))
        (message "No word under cursor"))))

  (defun viper-change-inner-word ()
    "Change the current word under the cursor, handling edge cases."
    (interactive)
    (viper-delete-inner-word)
    (viper-insert nil))

  (defun viper-yank-inner-word ()
    "Yank (copy) the current word under the cursor, handling edge cases."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds))
      (if bounds
          (kill-ring-save (car bounds) (cdr bounds))
        (message "No word under cursor"))))

  (defun viper-delete-inner-compound-word ()
    "Delete the entire compound word under the cursor, including `-` and `_`."
    (interactive)
    (let ((bounds (viper-compound-word-bounds)))
      (if bounds
          (kill-region (car bounds) (cdr bounds))
        (message "No compound word under cursor"))))

  (defun viper-change-inner-compound-word ()
    "Change the entire compound word under the cursor, including `-` and `_`."
    (interactive)
    (viper-delete-inner-compound-word)
    (viper-insert nil))

  (defun viper-yank-inner-compound-word ()
    "Yank the entire compound word under the cursor into the kill ring."
    (interactive)
    (let ((bounds (viper-compound-word-bounds)))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds))
      (if bounds
          (kill-ring-save (car bounds) (cdr bounds))
        (message "No compound word under cursor"))))

  (defun viper-compound-word-bounds ()
    "Get the bounds of a compound word under the cursor.
A compound word includes letters, numbers, `-`, and `_`."
    (save-excursion
      (let* ((start (progn
                      (skip-chars-backward "a-zA-Z0-9_-")
                      (point)))
             (end (progn
                    (skip-chars-forward "a-zA-Z0-9_-")
                    (point))))
        (when (< start end) (cons start end)))))

  (defun viper-go-to-nth-or-first-line (arg)
    "Go to the first line of the document, or the ARG-nth."
    (interactive "P")
    (if arg
        (viper-goto-line arg)
      (viper-goto-line 1))
    (pulse-momentary-highlight-region
     (line-beginning-position) (line-beginning-position 2)))

  (defun viper-go-to-last-line ()
    "Go to the last line of the document."
    (interactive)
    (goto-char (point-max)))

  (defun viper-window-split-horizontally ()
    "Split the window horizontally (mimics Vim's `C-w s`)."
    (interactive)
    (split-window-below)
    (other-window 1))

  (defun viper-window-split-vertically ()
    "Split the window vertically (mimics Vim's `C-w v`)."
    (interactive)
    (split-window-right)
    (other-window 1))

  (defun viper-window-close ()
    "Close the current window (mimics Vim's `C-w c`)."
    (interactive)
    (delete-window))

  (defun viper-window-maximize ()
    "Maximize the current window (mimics Vim's `C-w o`)."
    (interactive)
    (delete-other-windows))

  ;; Delete inside delimiters
  (define-key viper-vi-global-user-map (kbd "di(") (lambda () (interactive) (viper-delete-inside-delimiters ?\( ?\))))
  (define-key viper-vi-global-user-map (kbd "dib") (lambda () (interactive) (viper-delete-inside-delimiters ?\( ?\))))
  (define-key viper-vi-global-user-map (kbd "di{") (lambda () (interactive) (viper-delete-inside-delimiters ?{ ?})))
  (define-key viper-vi-global-user-map (kbd "di\"") (lambda () (interactive) (viper-delete-inside-delimiters ?\" ?\")))
  (define-key viper-vi-global-user-map (kbd "di'") (lambda () (interactive) (viper-delete-inside-delimiters ?' ?')))

  ;; Yank inside delimiters
  (define-key viper-vi-global-user-map (kbd "yi(") (lambda () (interactive) (viper-yank-inside-delimiters ?\( ?\))))
  (define-key viper-vi-global-user-map (kbd "yi{") (lambda () (interactive) (viper-yank-inside-delimiters ?{ ?})))
  (define-key viper-vi-global-user-map (kbd "yi\"") (lambda () (interactive) (viper-yank-inside-delimiters ?\" ?\")))
  (define-key viper-vi-global-user-map (kbd "yi'") (lambda () (interactive) (viper-yank-inside-delimiters ?' ?')))

  ;; Delete/Yank current word
  (define-key viper-vi-global-user-map (kbd "diw") 'viper-delete-inner-word)
  (define-key viper-vi-global-user-map (kbd "yiw") 'viper-yank-inner-word)
  (define-key viper-vi-global-user-map (kbd "ciw") 'viper-change-inner-word)
  (define-key viper-vi-global-user-map (kbd "diW") 'viper-delete-inner-compound-word)
  (define-key viper-vi-global-user-map (kbd "yiW") 'viper-yank-inner-compound-word)
  (define-key viper-vi-global-user-map (kbd "ciW") 'viper-change-inner-compound-word)

  ;; Beginning/End buffer
  (define-key viper-vi-global-user-map (kbd "G") 'viper-go-to-last-line)
  (define-key viper-vi-global-user-map (kbd "g") nil)
  (define-key viper-vi-global-user-map (kbd "gg") 'viper-go-to-nth-or-first-line)

  ;; Delete/Yank current line or region
  (define-key viper-vi-global-user-map (kbd "dd") 'viper-delete-line-or-region)
  (define-key viper-vi-global-user-map (kbd "yy") 'viper-yank-line-or-region)

  ;; Visual mode is actually marking
  (define-key viper-vi-global-user-map (kbd "v") 'viper-visual-select)
  (define-key viper-vi-global-user-map (kbd "V") 'viper-visual-select-line)

  ;; Movements by references and LSP
  (define-key viper-vi-global-user-map (kbd "gd") 'xref-find-references)
  (define-key viper-vi-global-user-map (kbd "SPC c a") 'eglot-code-actions)
  (define-key viper-vi-global-user-map (kbd "SPC s g") 'project-find-regexp)
  (define-key viper-vi-global-user-map (kbd "SPC s f") 'project-find-file)
  (define-key viper-vi-global-user-map (kbd "SPC m p") 'emacs-solo-movements/format-current-file)
  (global-set-key (kbd "C-o") 'xref-go-back)

  ;; Map `C-w` followed by specific keys to window commands in Viper
  (define-key viper-vi-global-user-map (kbd "C-w s") 'viper-window-split-horizontally)
  (define-key viper-vi-global-user-map (kbd "C-w v") 'viper-window-split-vertically)
  (define-key viper-vi-global-user-map (kbd "C-w c") 'viper-window-close)
  (define-key viper-vi-global-user-map (kbd "C-w o") 'viper-window-maximize)

  ;; Add navigation commands to mimic Vim's `C-w hjkl`
  (define-key viper-vi-global-user-map (kbd "C-w h") 'windmove-left)
  (define-key viper-vi-global-user-map (kbd "C-w l") 'windmove-right)
  (define-key viper-vi-global-user-map (kbd "C-w k") 'windmove-up)
  (define-key viper-vi-global-user-map (kbd "C-w j") 'windmove-down)

  ;; Indent region
  (define-key viper-vi-global-user-map (kbd "==") 'indent-region)

  ;; Word spelling
  (define-key viper-vi-global-user-map (kbd "z=") 'ispell-word)

  ;; Keybindings for buffer navigation and switching in Viper mode
  (define-key viper-vi-global-user-map (kbd "] b") 'next-buffer)
  (define-key viper-vi-global-user-map (kbd "[ b") 'previous-buffer)
  (define-key viper-vi-global-user-map (kbd "b l") 'switch-to-buffer)
  (define-key viper-vi-global-user-map (kbd "SPC SPC") 'switch-to-buffer)

  ;; Tabs (like in tmux tabs, not vscode tabs)
  (define-key viper-vi-global-user-map (kbd "C-w t") 'tab-bar-new-tab)
  (define-key viper-vi-global-user-map (kbd "] t") 'tab-next)
  (define-key viper-vi-global-user-map (kbd "[ t") 'tab-previous)

  ;; Flymake
  (define-key viper-vi-global-user-map (kbd "SPC x x") 'flymake-show-buffer-diagnostics)
  (define-key viper-vi-global-user-map (kbd "] d") 'flymake-goto-next-error)
  (define-key viper-vi-global-user-map (kbd "[ d") 'flymake-goto-prev-error)
  (define-key viper-vi-global-user-map (kbd "SPC t i") 'toggle-flymake-diagnostics-at-eol)

  ;; Gutter
  (define-key viper-vi-global-user-map (kbd "] c") 'emacs-solo/goto-next-hunk)
  (define-key viper-vi-global-user-map (kbd "[ c") 'emacs-solo/goto-previous-hunk))



;;; EMACS-SOLO-HIGHLIGHT-KEYWORDS-MODE
;;
;;  Highlights a list of words like TODO, FIXME...
;;  Code borrowed from `alternateved'
;;
(use-package emacs-solo-highlight-keywords-mode
  :if emacs-solo-enable-highlight-keywords
  :ensure nil
  :no-require t
  :defer t
  :init
  (defcustom +highlight-keywords-faces
    '(("TODO" . error)
      ("FIXME" . error)
      ("HACK" . warning)
      ("NOTE" . warning)
      ("HERE" . compilation-info)
      ("EMACS-31" . compilation-info))
    "Alist of keywords to highlight and their face."
    :group '+highlight-keywords
    :type '(alist :key-type (string :tag "Keyword")
                  :value-type (symbol :tag "Face"))
    :set (lambda (sym val)
           (dolist (face (mapcar #'cdr val))
             (unless (facep face)
               (error "Invalid face: %s" face)))
           (set-default sym val)))

  (defvar +highlight-keywords--keywords
    (when +highlight-keywords-faces
      (let ((keywords (mapcar #'car +highlight-keywords-faces)))
        `((,(regexp-opt keywords 'words)
           (0 (when (nth 8 (syntax-ppss))
                (cdr (assoc (match-string 0) +highlight-keywords-faces)))
              prepend)))))
    "Keywords and corresponding faces for `emacs-solo/highlight-keywords-mode'.")

  (defun emacs-solo/highlight-keywords-mode-on ()
    (font-lock-add-keywords nil +highlight-keywords--keywords t)
    (font-lock-flush))

  (defun emacs-solo/highlight-keywords-mode-off ()
    (font-lock-remove-keywords nil +highlight-keywords--keywords)
    (font-lock-flush))

  (define-minor-mode emacs-solo/highlight-keywords-mode
    "Highlight TODO and similar keywords in comments and strings."
    :lighter " +HL"
    :group '+highlight-keywords
    (if emacs-solo/highlight-keywords-mode
        (emacs-solo/highlight-keywords-mode-on)
      (emacs-solo/highlight-keywords-mode-off)))

  :hook
  (prog-mode-hook . (lambda () (run-at-time "1 sec" nil #'emacs-solo/highlight-keywords-mode-on))))


;;; EMACS-SOLO-GUTTER
;;
;;  A **HIGHLY** `experimental' and slow and buggy git gutter like.
;;
(use-package emacs-solo-gutter
  :if emacs-solo-enable-buffer-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/goto-next-hunk ()
    "Jump cursor to the closest next hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (next-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the next closest line number
                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)
              ;; If the current line is in the list, find the next line number that is not consecutive
              (let ((last-line nil))
                (cl-loop for line in sorted-line-numbers
                         when (and (> line current-line)
                                   (or (not last-line)
                                       (/= line (1+ last-line))))
                         return line
                         do (setq last-line line))))))

      (when next-line-number
        (goto-line next-line-number))))

  (defun emacs-solo/goto-previous-hunk ()
    "Jump cursor to the closest previous hunk."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (line-numbers (mapcar #'car git-gutter-diff-info))
           (sorted-line-numbers (sort line-numbers '<))
           (previous-line-number
            (if (not (member current-line sorted-line-numbers))
                ;; If the current line is not in the list, find the previous closest line number
                (cl-find-if (lambda (line) (< line current-line)) (reverse sorted-line-numbers))
              ;; If the current line is in the list, find the previous line number that has no direct predecessor
              (let ((previous-line nil))
                (dolist (line sorted-line-numbers)
                  (when (and (< line current-line)
                             (not (member (1- line) line-numbers)))
                    (setq previous-line line)))
                previous-line))))

      (when previous-line-number
        (goto-line previous-line-number))))


  (defun emacs-solo/git-gutter-process-git-diff ()
    "Process git diff for adds/mods/removals.
Marks lines as added, deleted, or changed."
    (interactive)
    (setq-local result '())
    (let* ((file-path (buffer-file-name))
           (grep-command "rg -Po")                         ; for rgrep
           ;; (grep-command (if (eq system-type 'darwin)   ; for grep / ggrep
           ;;                   "ggrep -Po"
           ;;                 "grep -Po"))
           (output (shell-command-to-string
                    (format
                     "git diff --unified=0 %s | %s '^@@ -[0-9]+(,[0-9]+)? \\+\\K[0-9]+(,[0-9]+)?(?= @@)'"
                     file-path
                     grep-command))))
      (setq-local lines (split-string output "\n"))
      (dolist (line lines)
        (if (string-match "\\(^[0-9]+\\),\\([0-9]+\\)\\(?:,0\\)?$" line)
            (let ((num (string-to-number (match-string 1 line)))
                  (count (string-to-number (match-string 2 line))))
              (if (= count 0)
                  (add-to-list 'result (cons (+ 1 num) "deleted"))
                (dotimes (i count)
                  (add-to-list 'result (cons (+ num i) "changed")))))
          (if (string-match "\\(^[0-9]+\\)$" line)
              (add-to-list 'result (cons (string-to-number line) "added"))))
        (setq-local git-gutter-diff-info result))
      result))


  (defun emacs-solo/git-gutter-add-mark (&rest args)
    "Add symbols to the left margin based on Git diff statuses.
   - '+' for added lines (lightgreen)
   - '~' for changed lines (yellowish)
   - '-' for deleted lines (tomato)."
    (interactive)
    (set-window-margins (selected-window) 2 0) ;; change to 1,2,3 if you want more columns
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (let ((lines-status (or (emacs-solo/git-gutter-process-git-diff) '())))
      (save-excursion
        (dolist (line-status lines-status)
          (let ((line-num (car line-status))
                (status (cdr line-status)))
            (when (and line-num status)
              (goto-char (point-min))
              (forward-line (1- line-num))
              (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))
                (overlay-put overlay 'emacs-solo--git-gutter-overlay t)
                (overlay-put overlay 'before-string
                             (propertize " "
                                         'display
                                         `((margin left-margin)
                                           ,(propertize
                                             (cond                              ;; Alternatives:
                                              ((string= status "added")   "+")  ;; +  │ ▏┃
                                              ((string= status "changed") "~")  ;; ~
                                              ((string= status "deleted") "_")) ;; _
                                             'face
                                             `(:foreground
                                               ,(cond
                                                 ((string= status "added") "gray") ;; lightgreen
                                                 ((string= status "changed") "gray") ;; gold
                                                 ((string= status "deleted") "gray")))))))))))))) ;; tomato

  (defun emacs-solo/timed-git-gutter-on()
    (run-at-time 0.1 nil #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-off ()
    "Remove all `emacs-solo--git-gutter-overlay' marks and other overlays."
    (interactive)
    (set-window-margins (selected-window) 2 0)
    (remove-overlays (point-min) (point-max) 'emacs-solo--git-gutter-overlay t)
    (remove-hook 'find-file-hook #'emacs-solo-git-gutter-on)
    (remove-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (defun emacs-solo/git-gutter-on ()
    (interactive)
    (emacs-solo/git-gutter-add-mark)
    (add-hook 'find-file-hook #'emacs-solo/timed-git-gutter-on)
    (add-hook 'after-save-hook #'emacs-solo/git-gutter-add-mark))

  (global-set-key (kbd "M-9") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "M-0") 'emacs-solo/goto-next-hunk)
  (global-set-key (kbd "C-c g p") 'emacs-solo/goto-previous-hunk)
  (global-set-key (kbd "C-c g r") 'emacs-solo/git-gutter-off)
  (global-set-key (kbd "C-c g g") 'emacs-solo/git-gutter-on)
  (global-set-key (kbd "C-c g n") 'emacs-solo/goto-next-hunk)

  (add-hook 'after-init-hook #'emacs-solo/git-gutter-on))


;;; EMACS-SOLO-ACE-WINDOW
;;
;;  Based on: https://www.reddit.com/r/emacs/comments/1h0zjvq/comment/m0uy3bo/?context=3
;;
;;  TODO: implement ace-swap like feature
(use-package emacs-solo-ace-window
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-ace-window/quick-window-overlays nil
    "List of overlays used to temporarily display window labels.")

  (defun emacs-solo-ace-window/quick-window-jump ()
    "Jump to a window by typing its assigned character label.
Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
    (interactive)
    (let* ((window-list (emacs-solo-ace-window/get-windows))
           (window-keys (seq-take '("1" "2" "3" "4" "5" "6" "7" "8")
                                  (length window-list)))
           (window-map (cl-pairlis window-keys window-list)))
      (emacs-solo-ace-window/add-window-key-overlays window-map)
      (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
        (emacs-solo-ace-window/remove-window-key-overlays)
        (if-let* ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)
          (message "No window assigned to key: %c" key)))))

  (defun emacs-solo-ace-window/get-windows ()
    "Return a list of windows in the current frame, ordered from top to bottom, left to right."
    (sort (window-list nil 'no-mini)
          (lambda (w1 w2)
            (let ((edges1 (window-edges w1))
                  (edges2 (window-edges w2)))
              (or (< (car edges1) (car edges2)) ; Compare top edges
                  (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                       (< (cadr edges1) (cadr edges2))))))))

  (defun emacs-solo-ace-window/add-window-key-overlays (window-map)
    "Add temporary overlays to windows with their assigned key labels from WINDOW-MAP."
    (setq emacs-solo-ace-window/quick-window-overlays nil)
    (dolist (entry window-map)
      (let* ((key (car entry))
             (window (cdr entry))
             (start (window-start window))
             (overlay (make-overlay start start (window-buffer window))))
        (overlay-put overlay 'after-string
                     (propertize (format " [%s] " key)
                                 'face '(:foreground "#c3e88d"
                                                     :background "#232635"
                                                     :weight bold
                                                     :height default)))
        (overlay-put overlay 'window window)
        (push overlay emacs-solo-ace-window/quick-window-overlays))))

  (defun emacs-solo-ace-window/remove-window-key-overlays ()
    "Remove all temporary overlays used to display key labels in windows."
    (mapc 'delete-overlay emacs-solo-ace-window/quick-window-overlays)
    (setq emacs-solo-ace-window/quick-window-overlays nil))

  (global-set-key (kbd "M-O") #'emacs-solo-ace-window/quick-window-jump))


;;; EMACS-SOLO-OLIVETTI
;;
(use-package emacs-solo-olivetti
  :if emacs-solo-enable-olivetti
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo-center-document-desired-width 120
    "The desired width of a document centered in the window.")

  (defun emacs-solo/center-document--adjust-margins ()
    ;; Reset margins first before recalculating
    (set-window-parameter nil 'min-margins nil)
    (set-window-margins nil nil)

    ;; Adjust margins if the mode is on
    (when emacs-solo/center-document-mode
      (let ((margin-width (max 0
                               (truncate
                                (/ (- (window-width)
                                      emacs-solo-center-document-desired-width)
                                   2.0)))))
        (when (> margin-width 0)
          (set-window-parameter nil 'min-margins '(0 . 0))
          (set-window-margins nil margin-width margin-width)))))

  (define-minor-mode emacs-solo/center-document-mode
    "Toggle centered text layout in the current buffer."
    :lighter " Centered"
    :group 'editing
    (if emacs-solo/center-document-mode
        (add-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'append 'local)
      (remove-hook 'window-configuration-change-hook #'emacs-solo/center-document--adjust-margins 'local))
    (emacs-solo/center-document--adjust-margins))


  ;; (add-hook 'org-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-group-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-summary-mode-hook #'emacs-solo/center-document-mode)
  (add-hook 'gnus-article-mode-hook #'emacs-solo/center-document-mode)

  ;; (add-hook 'newsticker-treeview-list-mode-hook 'emacs-solo/timed-center-visual-fill-on)
  ;; (add-hook 'newsticker-treeview-item-mode-hook 'emacs-solo/timed-center-visual-fill-on)

  :bind ("<f1>" . #'emacs-solo/center-document-mode)
  )


;;; EMACS-SOLO-0x0
;;
;; Inspired by: https://codeberg.org/daviwil/dotfiles/src/branch/master/Emacs.org#headline-28
(use-package emacs-solo-0x0
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/0x0-upload-text ()
    (interactive)
    (let* ((contents (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-string)))
           (temp-file (make-temp-file "0x0" nil ".txt" contents)))
      (message "Sending %s to 0x0.st..." temp-file)
      (let ((url (string-trim-right
                  (shell-command-to-string
                   (format "curl -s -F'file=@%s' https://0x0.st" temp-file)))))
        (message "The URL is %s" url)
        (kill-new url)
        (delete-file temp-file))))

  (defun emacs-solo/0x0-upload-file (file-path)
    (interactive "fSelect a file to upload: ")
    (message "Sending %s to 0x0.st..." file-path)
    (let ((url (string-trim-right
                (shell-command-to-string
                 (format "curl -s -F'file=@%s' https://0x0.st" (expand-file-name file-path))))))
      (message "The URL is %s" url)
      (kill-new url))))


;;; EMACS-SOLO-SUDO-EDIT
;;
;; Inspired by: https://codeberg.org/daviwil/dotfiles/src/branch/master/Emacs.org#headline-28
(use-package emacs-solo-sudo-edit
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (completing-read "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))


;;; EMACS-SOLO-REPLACE-AS-DIFF
;;
(use-package emacs-solo/replace-regexp-as-diff
  :ensure nil
  :no-require t
  :defer t
  :init

  ;; NOTE: improvements wrappers over `multi-file-replace-regexp-as-diff', so
  ;;       we can:
  ;;       1.) Use it with glob pattern matching in files, including inside
  ;;           subfolders (`emacs-solo/multi-file-replace-regexp-as-diff-with-glob')
  ;;       2.) Use it with marked files and or directories in dired
  ;;           (`emacs-solo/dired-do-replace-regexp-as-diff')


  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A wrapper for `multi-file-replace-regexp-as-diff' that extends its functionality
  ;; to support glob patterns for file matching. It recursively searches all files
  ;; in the specified directory (including subdirectories) that match the given glob
  ;; pattern (e.g., `*.js`), and displays the replacements as diffs in the
  ;; `*replace-diff*` buffer. This allows for easy review and application of changes
  ;; across multiple files.
  (defun emacs-solo/multi-file-replace-regexp-as-diff-with-glob (dir regexp to-string &optional delimited glob-pattern)
    "Wrapper for `multi-file-replace-regexp-as-diff` that accepts a directory and a glob pattern.
DIR is the directory to search recursively.
REGEXP is the regular expression to replace.
TO-STRING is the replacement string.
DELIMITED is an optional argument passed to `multi-file-replace-regexp-as-diff`.
GLOB-PATTERN is the glob pattern to match files (e.g., \"*.el\")."
    (interactive
     (let ((dir (file-truename (read-directory-name "Directory: ")))
           (common (query-replace-read-args
                    (concat "Replace"
                            (if current-prefix-arg " word" "")
                            " regexp as diff in files")
                    t t))
           (glob-pattern (read-string "Glob pattern (e.g., *.el): " "*")))
       (list dir (nth 0 common) (nth 1 common) (nth 2 common) glob-pattern)))

    (let* ((glob-regexp (wildcard-to-regexp glob-pattern))
           ;; file-expand-wildcards instead of directory-files-recursively, would
           ;; not allow us to traverse directories
           (files (directory-files-recursively dir glob-regexp)))

      (if files
          (multi-file-replace-regexp-as-diff files regexp to-string delimited)
        (message "No files found for glob-pattern: %s" glob-pattern))))


  ;; `M-x dired RET' mark files and/or directories then
  ;; `M-x emacs-solo/multi-file-replace-regexp-as-diff-with-glob RET'
  ;;
  ;; A version of `dired-do-replace-regexp-as-diff' that adds support for selected
  ;; directories in Dired. When directories are marked, it recursively includes all
  ;; files within them (and their subdirectories) in the replacement operation.
  ;; The replacements are displayed as diffs in the `*replace-diff*` buffer, allowing
  ;; for review and application of changes across multiple files and directories.
  (defun emacs-solo/expand-directories (items)
    "Expand ITEMS to include all files within directories (recursively).
Directories themselves are excluded from the final list."
    (cl-loop for item in items
             if (file-directory-p item)
             append (let ((files (directory-files-recursively item ".*" t)))
                      (cl-remove-if #'file-directory-p files))
             else if (file-regular-p item) ; Ensure only regular files are included
             collect item))
  (defun emacs-solo/dired-do-replace-regexp-as-diff (from to &optional delimited)
    "Do `replace-regexp' of FROM with TO as diff, on all marked files and directories.
If a marked item is a directory, all files within it (recursively) are included.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
    (interactive
     (let ((common
            (query-replace-read-args
             "Replace regexp as diff in marked files and directories" t t t)))
       (list (nth 0 common) (nth 1 common) (nth 2 common))))
    (dired-post-do-command)
    (let* ((marked-items (dired-get-marked-files)) ; Include directories in the list
           (files (emacs-solo/expand-directories marked-items)))
      (if files
          (progn
            (multi-file-replace-regexp-as-diff files from to delimited))
        (message "No files found in marked items.")))))

;;; EMACS-SOLO-WEATHER
;;
(use-package emacs-solo-weather
  :ensure nil
  :no-require t
  :defer t
  :init
  (setq emacs-solo-weather-city "Indaiatuba")

  (defun emacs-solo/weather-buffer ()
    "Open a new Emacs buffer and asynchronously fetch wttr.in weather data."
    (interactive)
    (let* ((city (shell-quote-argument emacs-solo-weather-city))
           (buffer (get-buffer-create "*Weather*"))
           (url1 (format "curl -s 'wttr.in/%s'" city))
           (url2 (format "curl -s 'v2d.wttr.in/%s'" city)))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "Fetching weather data...\n")
        (read-only-mode 1))
      (switch-to-buffer buffer)
      ;; Fetch both asynchronously
      (emacs-solo--fetch-weather url1 buffer)
      (emacs-solo--fetch-weather url2 buffer t)))

  (defun emacs-solo--fetch-weather (cmd buffer &optional second)
    "Run CMD asynchronously and insert results into BUFFER.
If SECOND is non-nil, separate the results with a newline."
    (make-process
     :name "weather-fetch"
     :buffer (generate-new-buffer " *weather-temp*")
     :command (list "sh" "-c" cmd)
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))
           (setq output (replace-regexp-in-string "^Follow.*\n" ""
                                                  (replace-regexp-in-string "[\x0f]" "" output)))
           (with-current-buffer buffer
             (read-only-mode -1)
             (when second (insert "\n\n"))
             (insert output)
             (ansi-color-apply-on-region (point-min) (point-max))
             (goto-char (point-min))
             (read-only-mode 1))))))))


;;; EMACS-SOLO-OLLAMA
;;
(use-package emacs-solo-ollama
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/ollama-run-model ()
    "Run `ollama list`, let the user choose a model, and open it in `ansi-term`.
Asks for a prompt when run. If none is passed (RET), starts it interactive.
If a region is selected, prompt for additional input and pass it as a query."
    (interactive)
    (let* ((output (shell-command-to-string "ollama list"))
           (models (let ((lines (split-string output "\n" t)))
                     (mapcar (lambda (line) (car (split-string line))) (cdr lines))))
           (selected (completing-read "Select Ollama model: " models nil t))
           (region-text (when (use-region-p)
                          (shell-quote-argument
                           (replace-regexp-in-string "\n" " "
                                                     (buffer-substring-no-properties
                                                      (region-beginning)
                                                      (region-end))))))
           (prompt (read-string "Ollama Prompt (leave it blank for interactive): " nil nil nil)))
      (when (and selected (not (string-empty-p selected)))
        (ansi-term "/bin/sh")
        (sit-for 1)
        (let ((args (list (format "ollama run %s"
                                  selected))))
          (when (and prompt (not (string-empty-p prompt)))
            (setq args (append args (list (format "\"%s\"" prompt)))))
          (when region-text
            (setq args (append args (list (format "\"%s\"" region-text)))))

          (term-send-raw-string (string-join args " "))
          (term-send-raw-string "\n"))))))


;;; EMACS-SOLO-DIRED-GUTTER
;;
(use-package emacs-solo-dired-gutter
  :if emacs-solo-enable-dired-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (setq emacs-solo-dired-gutter-enabled t)

  (defvar emacs-solo/dired-git-status-overlays nil
    "List of active overlays in Dired for Git status.")

  (defun emacs-solo/dired--git-status-face (code)
    "Return a cons cell (STATUS . FACE) for a given Git porcelain CODE."
    (let* ((git-status-untracked "??")
           (git-status-modified " M")
           (git-status-modified-alt "M ")
           (git-status-deleted "D ")
           (git-status-added "A ")
           (git-status-renamed "R ")
           (git-status-copied "C ")
           (git-status-ignored "!!")
           (status (cond
                    ((string-match-p "\\?\\?" code) git-status-untracked)
                    ((string-match-p "^ M" code) git-status-modified)
                    ((string-match-p "^M " code) git-status-modified-alt)
                    ((string-match-p "^D" code) git-status-deleted)
                    ((string-match-p "^A" code) git-status-added)
                    ((string-match-p "^R" code) git-status-renamed)
                    ((string-match-p "^C" code) git-status-copied)
                    ((string-match-p "\\!\\!" code) git-status-ignored)
                    (t "  ")))
           (face (cond
                  ((string= status git-status-ignored) 'shadow)
                  ((string= status git-status-untracked) 'warning)
                  ((string= status git-status-modified) 'font-lock-function-name-face)
                  ((string= status git-status-modified-alt) 'font-lock-function-name-face)
                  ((string= status git-status-deleted) 'error)
                  ((string= status git-status-added) 'success)
                  (t 'font-lock-keyword-face))))
      (cons status face)))

  (defun emacs-solo/dired-git-status-overlay ()
    "Overlay Git status indicators on the first column in Dired."
    (interactive)
    (require 'vc-git)
    (let ((git-root (ignore-errors (vc-git-root default-directory))))
      (when (and git-root
                 (not (file-remote-p default-directory))
                 emacs-solo-dired-gutter-enabled)
        (setq git-root (expand-file-name git-root))
        (let* ((git-status (vc-git--run-command-string nil "status" "--porcelain" "--ignored" "--untracked-files=normal"))
               (status-map (make-hash-table :test 'equal)))
          (mapc #'delete-overlay emacs-solo/dired-git-status-overlays)
          (setq emacs-solo/dired-git-status-overlays nil)

          (dolist (line (split-string git-status "\n" t))
            (when (string-match "^\\(..\\) \\(.+\\)$" line)
              (let* ((code (match-string 1 line))
                     (file (match-string 2 line))
                     (fullpath (expand-file-name file git-root))
                     (status-face (emacs-solo/dired--git-status-face code)))
                (puthash fullpath status-face status-map))))

          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((file (ignore-errors (expand-file-name (dired-get-filename nil t)))))
                (when file
                  (setq file (if (file-directory-p file) (concat file "/") file))
                  (let* ((status-face (gethash file status-map (cons "  " 'font-lock-keyword-face)))
                         (status (car status-face))
                         (face (cdr status-face))
                         (status-str (propertize (format " %s " status) 'face face))
                         (ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)))))
                    (overlay-put ov 'before-string status-str)
                    (push ov emacs-solo/dired-git-status-overlays))))
              (forward-line 1)))))))

  (add-hook 'dired-after-readin-hook #'emacs-solo/dired-git-status-overlay))


;;; EMACS-SOLO-DIRED-ICONS
;;
(use-package emacs-solo-dired-icons
  :if emacs-solo-enable-dired-icons
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo/dired-icons-file-icons
    '(("el" . "📜")      ("rb" . "💎")      ("js" . "⚙️")      ("ts" . "⚙️")
      ("json" . "🗂️")    ("md" . "📝")      ("txt" . "📝")     ("html" . "🌐")
      ("css" . "🎨")     ("scss" . "🎨")    ("png" . "🖼️")    ("jpg" . "🖼️")
      ("jpeg" . "🖼️")   ("gif" . "🖼️")    ("svg" . "🖼️")    ("pdf" . "📄")
      ("zip" . "📦")     ("tar" . "📦")     ("gz" . "📦")      ("bz2" . "📦")
      ("7z" . "📦")      ("org" . "📝")    ("sh" . "💻")      ("c" . "🔧")
      ("h" . "📘")       ("cpp" . "➕")     ("hpp" . "📘")     ("py" . "🐍")
      ("java" . "☕")    ("go" . "🌍")      ("rs" . "💨")      ("php" . "🐘")
      ("pl" . "🐍")      ("lua" . "🎮")     ("ps1" . "🔧")     ("exe" . "⚡")
      ("dll" . "🔌")     ("bat" . "⚡")      ("yaml" . "⚙️")    ("toml" . "⚙️")
      ("ini" . "⚙️")     ("csv" . "📊")     ("xls" . "📊")     ("xlsx" . "📊")
      ("sql" . "🗄️")    ("log" . "📝")     ("apk" . "📱")     ("dmg" . "💻")
      ("iso" . "💿")     ("torrent" . "⏳") ("bak" . "🗃️")    ("tmp" . "⚠️")
      ("desktop" . "🖥️") ("md5" . "🔐")     ("sha256" . "🔐")  ("pem" . "🔐")
      ("sqlite" . "🗄️")  ("db" . "🗄️")
      ("mp3" . "🎶")     ("wav" . "🎶")     ("flac" . "🎶")
      ("ogg" . "🎶")     ("m4a" . "🎶")     ("mp4" . "🎬")     ("avi" . "🎬")
      ("mov" . "🎬")     ("mkv" . "🎬")     ("webm" . "🎬")    ("flv" . "🎬")
      ("ico" . "🖼️")     ("ttf" . "🔠")     ("otf" . "🔠")     ("eot" . "🔠")
      ("woff" . "🔠")    ("woff2" . "🔠")   ("epub" . "📚")    ("mobi" . "📚")
      ("azw3" . "📚")    ("fb2" . "📚")     ("chm" . "📚")     ("tex" . "📚")
      ("bib" . "📚")     ("apk" . "📱")     ("rar" . "📦")     ("xz" . "📦")
      ("zst" . "📦")     ("tar.xz" . "📦")  ("tar.zst" . "📦") ("tar.gz" . "📦")
      ("tgz" . "📦")     ("bz2" . "📦")     ("mpg" . "🎬")     ("webp" . "🖼️")
      ("flv" . "🎬")     ("3gp" . "🎬")     ("ogv" . "🎬")     ("srt" . "🔠")
      ("vtt" . "🔠")     ("cue" . "📀"))
    "Icons for specific file extensions in Dired.")

  (defun emacs-solo/dired-icons-icon-for-file (file)
    (if (file-directory-p file)
        "📁"
      (let* ((ext (file-name-extension file))
             (icon (and ext (assoc-default (downcase ext) emacs-solo/dired-icons-file-icons))))
        (or icon "📄"))))

  (defun emacs-solo/dired-icons-icons-regexp ()
    "Return a regexp that matches any icon we use."
    (let ((icons (mapcar #'cdr emacs-solo/dired-icons-file-icons)))
      (concat "^\\(" (regexp-opt (cons "📁" icons)) "\\) ")))

  (defun emacs-solo/dired-icons-add-icons ()
    "Add icons to filenames in Dired buffer."
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t)
            (icon-regex (emacs-solo/dired-icons-icons-regexp)))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (condition-case nil
                (when-let* ((file (dired-get-filename nil t)))
                  (dired-move-to-filename)
                  (unless (looking-at-p icon-regex)
                    (insert (concat (emacs-solo/dired-icons-icon-for-file file) " "))))
              (error nil))  ;; gracefully skip invalid lines
            (forward-line 1))))))

  (add-hook 'dired-after-readin-hook #'emacs-solo/dired-icons-add-icons))


;;; EMACS-SOLO-CONTAINER
;;
;;  A proto 'control panel' for basic container management (docker and podman based)
;;
(use-package emacs-solo-container
  :ensure nil
  :no-require t
  :defer t
  :init
  (require 'transient)
  (require 'project)

  (defvar container-backend 'podman
    "Current container backend. Either 'docker or 'podman.")

  (defvar container-profile 'dev
    "Current profile: either 'prod or 'dev.")

  (defconst container-buffer-name "*container*"
    "Buffer name for container command output.")

  (defvar container--process nil)

  (defun container-toggle-backend ()
    "Toggle between Docker and Podman."
    (interactive)
    (setq container-backend (if (eq container-backend 'docker) 'podman 'docker))
    (message "Switched to backend: %s" container-backend))

  (defun container-toggle-profile ()
    "Toggle between prod and dev profiles."
    (interactive)
    (setq container-profile (if (eq container-profile 'prod) 'dev 'prod))
    (message "Switched to profile: %s" container-profile))

  (defun container--command ()
    "Return the container backend command."
    (pcase container-backend
      ('docker "docker")
      ('podman "podman")
      (_ (error "Unknown backend: %s" container-backend))))

  (defun container--compose-command ()
    "Return the container backend compose command."
    (pcase container-backend
      ('docker "docker compose")
      ('podman "podman compose")
      (_ (error "Unknown backend: %s" container-backend))))

  (defun container--project-name ()
    "Return the base name of the current project or buffer."
    (let* ((project (project-current))
           (name (if project
                     (file-name-nondirectory (directory-file-name (project-root project)))
                   (file-name-base (or buffer-file-name default-directory)))))
      (downcase name)))

  (defun container--dockerfile ()
    "Return the appropriate Dockerfile path based on profile and context."
    (let ((base (container--project-name)))
      (pcase container-profile
        ('prod (or (car (file-expand-wildcards (format "%s.Dockerfile" base))) "Dockerfile"))
        ('dev  (or (car (file-expand-wildcards (format "%s.Dockerfile.dev" base))) "Dockerfile.dev")))))

  (defun container--compose-file ()
    "Return the appropriate docker-compose file path based on profile and context."
    (let ((project-root (or (project-root (project-current)) default-directory))  ;; Get the project root
          (base (container--project-name)))
      (pcase container-profile
        ('prod (or (car (file-expand-wildcards (format "%s.docker-compose.yml" base))) (concat project-root "docker-compose.yml")))
        ('dev  (or (car (file-expand-wildcards (format "%s.docker-compose-dev.yml" base))) (concat project-root "docker-compose-dev.yml"))))))

  (defun container--run-to-buffer (cmd)
    "Run CMD in a buffer named `*container*` with `comint-mode`."
    (interactive "sCommand: ")
    (let ((buf (get-buffer-create container-buffer-name)))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (comint-mode)
        (ansi-color-for-comint-mode-on))
      (setq container--process
            (start-process-shell-command "container" buf cmd))
      (set-process-filter container--process 'comint-output-filter)
      (display-buffer buf)))

  (defun container--run (args)
    "Run a container command with ARGS."
    (container--run-to-buffer (format "%s %s" (container--command) args)))

  (defun container--run-compose (args)
    "Run a compose command with ARGS using profile-specific file."
    (container--run-to-buffer
     (format "%s -f %s %s"
             (container--compose-command)
             (container--compose-file)
             args)))

  (defun container--run-interactive (prompt action)
    "Prompt for a container/image name and run ACTION with it."
    (let ((name (read-string prompt)))
      (container--run (format "%s %s" action name))))

  ;; === Container actions ===
  (defun container-list () (interactive) (container--run "ps -a"))
  (defun container-start () (interactive) (container--run-interactive "Start container: " "start"))
  (defun container-stop () (interactive) (container--run-interactive "Stop container: " "stop"))
  (defun container-restart () (interactive) (container--run-interactive "Restart container: " "restart"))
  (defun container-remove () (interactive) (container--run-interactive "Remove container: " "rm"))
  (defun container-logs () (interactive) (container--run-interactive "Logs for container: " "logs"))

  ;; === Image actions ===
  (defun container-list-images () (interactive) (container--run "images"))
  (defun container-pull-image () (interactive) (container--run-interactive "Pull image: " "pull"))

  (defun container-build-image ()
    (interactive)
    (let ((image (read-string "Tag image as: "))
          (file (container--dockerfile)))
      (container--run-to-buffer
       (format "%s build -f %s -t %s ." (container--command) file image))))

  ;; === Compose actions ===
  (defun container-compose-up () (interactive) (container--run-compose "up -d"))
  (defun container-compose-down () (interactive) (container--run-compose "down"))
  (defun container-compose-logs () (interactive) (container--run-compose "logs"))
  (defun container-compose-ps () (interactive) (container--run-compose "ps"))
  (defun container-compose-build () (interactive) (container--run-compose "build"))
  (defun container-compose-restart () (interactive) (container--run-compose "restart"))

  (defun container-kill-buffer ()
    "Kill the *container* buffer."
    (interactive)
    (let ((buf (get-buffer container-buffer-name)))
      (when buf
        (kill-buffer buf))))

  ;; === Transient menu ===
  (transient-define-prefix container-menu ()
    "Container and Compose management menu."
    [["Backend/Profile"
      ("b" (lambda () (format "Toggle backend (%s)" container-backend))
       container-toggle-backend :transient t)
      ("p" (lambda () (format "Toggle profile (%s)" container-profile))
       container-toggle-profile :transient t)
      ("q" "Kill output buffer" container-kill-buffer :transient t)]
     ["Containers"
      ("l" "List containers" container-list :transient t)
      ("s" "Start container" container-start :transient t)
      ("t" "Stop container" container-stop :transient t)
      ("r" "Restart container" container-restart :transient t)
      ("R" "Remove container" container-remove :transient t)
      ("L" "Logs" container-logs :transient t)]
     ["Images"
      ("i" "List images" container-list-images :transient t)
      ("P" "Pull image" container-pull-image :transient t)
      ("B" "Build image" container-build-image :transient t)]
     ["Compose"
      ("u" "Compose up" container-compose-up :transient t)
      ("d" "Compose down" container-compose-down :transient t)
      ("c" "Compose ps" container-compose-ps :transient t)
      ("C" "Compose build" container-compose-build :transient t)
      ("x" "Compose restart" container-compose-restart :transient t)
      ("g" "Compose logs" container-compose-logs :transient t)]
     ])

  (global-set-key (kbd "C-c d") #'container-menu))


;;; EMACS-SOLO-MPV-PLAYER
;;
;; TLDR: M-x dired
;;       mark files with `m'
;;       C-c m to to open the music player with the selected files
;;
(use-package emacs-solo-mpv-player
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo/mpv-process nil
    "Process object for the currently running mpv instance.")

  (defvar emacs-solo/mpv-ipc-socket "/tmp/mpv-socket"
    "Path to mpv's IPC UNIX domain socket.")

  (defun emacs-solo/mpv-play-files ()
    "Play marked files in Dired using mpv with IPC."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in a Dired buffer"))
    (let ((files (dired-get-marked-files)))
      (when (file-exists-p emacs-solo/mpv-ipc-socket)
        (delete-file emacs-solo/mpv-ipc-socket))
      (when (process-live-p emacs-solo/mpv-process)
        (kill-process emacs-solo/mpv-process))
      (setq emacs-solo/mpv-process
            (apply #'start-process
                   "mpv" "*mpv*"
                   "mpv"
                   "--force-window=yes"
                   (concat "--input-ipc-server=" emacs-solo/mpv-ipc-socket)
                   files))))

  (defun emacs-solo/mpv-stop ()
    "Stop mpv playback."
    (interactive)
    (when (process-live-p emacs-solo/mpv-process)
      (kill-process emacs-solo/mpv-process)
      (setq emacs-solo/mpv-process nil)))

  (defun emacs-solo/mpv-send-command (json-cmd)
    "Send JSON-CMD to mpv's IPC socket directly."
    (let ((socket emacs-solo/mpv-ipc-socket))
      (if (file-exists-p socket)
          (let ((proc (make-network-process
                       :name "mpv-ipc"
                       :family 'local
                       :service socket
                       :nowait t)))
            (process-send-string proc (concat json-cmd "\n"))
            (delete-process proc))
        (message "❌ mpv IPC socket not found at %s" socket))))


(defun emacs-solo/mpv-show-playlist ()
  "Show the current mpv playlist in a readable buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mpv-playlist*"))
        (socket emacs-solo/mpv-ipc-socket)
        (output ""))
    (if (file-exists-p socket)
        (let ((proc
               (make-network-process
                :name "mpv-ipc-playlist"
                :family 'local
                :service socket
                :nowait nil
                :filter (lambda (_proc chunk)
                          (setq output (concat output chunk))))))
          (process-send-string proc
            "{\"command\": [\"get_property\", \"playlist\"]}\n")
          (sleep-for 0.1)
          (delete-process proc)

          (with-current-buffer buf
            (let ((inhibit-read-only t)
                  (json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (erase-buffer)
              (let* ((json-data (ignore-errors (json-read-from-string output)))
                     (playlist (alist-get 'data json-data)))
                (if playlist
                    (progn
                      (insert "🎵 MPV Playlist:\n\n")
                      (cl-loop for i from 0
                               for entry in playlist do
                               (insert
                                (format "%s %s. %s\n"
                                        (if (eq (alist-get 'current entry) t)
                                            "now playing ➡️ " "")
                                        (1+ i)
                                        (alist-get 'filename entry)
                                        ))))
                  (insert "❌ Failed to parse playlist or playlist is empty."))))
            (special-mode)
            (goto-char (point-min)))
          (display-buffer buf))
      (message "❌ mpv IPC socket not found at %s" socket))))

  (require 'transient)

  (transient-define-prefix emacs-solo/mpv-transient ()
    "🎵 MPV Controls"
    [[" 🔅 Controls"
      ("p" "⏸️ Pause/Resume"
       (lambda () (interactive)
         (emacs-solo/mpv-send-command
          "{\"command\": [\"cycle\", \"pause\"]}")))
      ("x" "⏹️ Stop" emacs-solo/mpv-stop)
      ("n" "⏭️ Next"
       (lambda () (interactive)
         (emacs-solo/mpv-send-command
          "{\"command\": [\"playlist-next\"]}")))
      ("b" "⏮️ Previous"
       (lambda () (interactive)
         (emacs-solo/mpv-send-command
          "{\"command\": [\"playlist-prev\"]}")))
      ("l" "🔁 Loop"
       (lambda () (interactive)
         (emacs-solo/mpv-send-command
          "{\"command\": [\"cycle\", \"loop\"]}")))]
     [" 🎧 Playback"
      ("RET" "▶️ Play marked" emacs-solo/mpv-play-files)
      ("L"   "▶️ List playlist" emacs-solo/mpv-show-playlist)]])

  (defun emacs-solo/mpv-dired-setup ()
    (global-set-key (kbd "C-c m") #'emacs-solo/mpv-transient))

  (add-hook 'dired-mode-hook #'emacs-solo/mpv-dired-setup))


;;; EMACS-SOLO-M3U-VISUALIZER (& Online Radio Player)
;;
;; TLDR: C-c r (select an online radio list to download)
;;       RET - play with mpv
;;       x   - stop with mpv
;;
(use-package emacs-solo-m3u-visualizer
  :ensure nil
  :no-require t
  :defer t
  :init
  (defvar emacs-solo/m3u-radio-sources
    '(("Full List" . "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/refs/heads/main/---everything-full.m3u")
      ("60s" . "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/refs/heads/main/60s.m3u")
      ("70s" . "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/refs/heads/main/70s.m3u")
      ("80s" . "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/refs/heads/main/80s.m3u")
      ("90s" . "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/refs/heads/main/90s.m3u"))
    "Alist of named M3U radio sources.")

  (defun emacs-solo/get-online-radio-list-m3u ()
    "Select and download M3U playlist, then visualize it using `m3u-visualizer-mode'."
    (interactive)
    (let* ((choice (completing-read "Choose your Online Radio playlist: " emacs-solo/m3u-radio-sources))
           (url (cdr (assoc choice emacs-solo/m3u-radio-sources)))
           (dest-buffer (get-buffer-create "*M3U Radio List*")))
      (url-retrieve
       url
       (lambda (_status)
         (goto-char (point-min))
         (when (re-search-forward "\n\n" nil t)
           (let* ((body-start (point))
                  (raw (buffer-substring-no-properties body-start (point-max)))
                  (decoded (decode-coding-string raw 'utf-8)))
             (with-current-buffer dest-buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert decoded)
                 (goto-char (point-min))
                 (m3u-visualize-buffer)))))
         (kill-buffer (current-buffer))))))

  (global-set-key (kbd "C-c r") #'emacs-solo/get-online-radio-list-m3u)


  (defvar m3u-visualizer-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'m3u-visualizer-next)
      (define-key map (kbd "p") #'m3u-visualizer-prev)
      (define-key map (kbd "RET") #'m3u-visualizer-play-current)
      (define-key map (kbd "x") #'m3u-visualizer-stop-mpv)

      map)
    "Keymap for `m3u-visualizer-mode'.")

  (define-derived-mode m3u-visualizer-mode special-mode "M3U-Visualizer"
    "Major mode for viewing M3U playlist as a styled table."
    (buffer-disable-undo)
    (setq truncate-lines t))

  (defvar-local m3u-visualizer--entries nil
    "List of parsed entries (title group logo url).")

  (defvar m3u-visualizer--mpv-process nil
    "Holds the current mpv process instance.")

  (defun m3u-visualizer--format-entry (entry)
    "Return a propertized string for ENTRY."
    (let ((title (propertize (truncate-string-to-width (nth 0 entry) 50 nil ?\s)
                             'face 'font-lock-function-name-face))
          (group (propertize (truncate-string-to-width (nth 1 entry) 20 nil ?\s)
                             'face 'font-lock-keyword-face))
          (logo (propertize (truncate-string-to-width (nth 2 entry) 40 nil ?\s)
                            'face 'font-lock-string-face))
          (url   (propertize (nth 3 entry) 'face 'font-lock-comment-face)))
      (format "%s  %s  %s  %s" title group logo url)))

  (defun m3u-visualizer--collect-entries ()
    "Return parsed entries from the current buffer."
    (let ((entries '()))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                ;; Match lines like: #EXTINF:-1 [optional attributes], Title
                "^#EXTINF:-1\\(?:\\s-+\\([^,]+\\)\\)?[ \t]*,[ \t]*\\(.*?\\)[ \t]*[\r\n]+\\(http[^\r\n]+\\)"
                nil t)
          (let* ((attr-str (match-string 1))
                 (title (match-string 2))
                 (url (match-string 3))
                 (logo "")
                 (group ""))
            ;; Optionally extract logo and group-title from attributes
            (when attr-str
              (when (string-match "tvg-logo=\"\\([^\"]*\\)\"" attr-str)
                (setq logo (match-string 1 attr-str)))
              (when (string-match "group-title=\"\\([^\"]*\\)\"" attr-str)
                (setq group (match-string 1 attr-str))))
            (push (list title group logo url) entries))))
      (reverse entries)))

  (defun m3u-visualize-buffer ()
    "Visualize current M3U playlist in a formatted buffer."
    (interactive)
    (let ((entries (m3u-visualizer--collect-entries)))
      (with-current-buffer (get-buffer-create "*M3U Playlist*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq m3u-visualizer--entries entries)
          (dolist (entry entries)
            (insert (m3u-visualizer--format-entry entry) "\n"))
          (goto-char (point-min))
          (m3u-visualizer-mode))
        (pop-to-buffer (current-buffer)))))

  (defun m3u-visualizer-current-url ()
    "Extract the actual stream URL from the current line (last column)."
    (save-excursion
      (beginning-of-line)
      (let ((line (buffer-substring (line-beginning-position)
                                    (line-end-position))))
        ;; Match last http URL in line
        (when (string-match "\\(http[^\s]+\\)$" line)
          (match-string 1 line)))))

  (defun m3u-visualizer-play-current ()
    "Play the stream URL at point using mpv asynchronously.
If a stream is already playing, kill it before starting a new one."
    (interactive)
    (let ((url (m3u-visualizer-current-url)))
      (if url
          (progn
            (when (and m3u-visualizer--mpv-process
                       (process-live-p m3u-visualizer--mpv-process))
              (kill-process m3u-visualizer--mpv-process)
              (message "Stopped previous mpv stream."))
            (setq m3u-visualizer--mpv-process
                  (start-process "mpv-stream" "*mpv*" "mpv" url))
            (message "Playing stream: %s" url))
        (message "No stream URL on this line."))))

  (defun m3u-visualizer-stop-mpv ()
    "Stop the currently playing mpv process."
    (interactive)
    (if (and m3u-visualizer--mpv-process
             (process-live-p m3u-visualizer--mpv-process))
        (progn
          (kill-process m3u-visualizer--mpv-process)
          (setq m3u-visualizer--mpv-process nil)
          (message "Stopped mpv."))
      (message "No mpv process running.")))

  (defun m3u-visualizer-next ()
    "Go to next entry line."
    (interactive)
    (forward-line 1))

  (defun m3u-visualizer-prev ()
    "Go to previous entry line."
    (interactive)
    (forward-line -1)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ;; ("org" . "https://orgmode.org/elpa/")
                        ("gnu" . "https://elpa.gnu.org/packages/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package async
  :config (setq async-bytecomp-package-mode 1))

;;; UTILITIES
(use-package utilities
  :load-path "~/.emacs.d/elisp"
  :demand t)  ;; or `:demand t` if you want to load it immediately

;;; DOOM-MODELINE
;; (use-package doom-modeline
;;   :ensure t
;;   ;; :hook (after-init . doom-modeline-mode))
;;   :init (doom-modeline-mode 1))

(use-package hide-lines
  :ensure t)

(use-package hide-mode-line
  :ensure t
  :defer t)

;; (setq doom-modeline-icon nil)
;; (setq doom-modeline-enable-word-count nil)
;; (setq doom-modeline-time-analogue-clock nil)
;; (setq doom-modeline-position-line-format nil)
;; (setq doom-modeline-buffer-encoding nil)
;; (setq doom-modeline-percent-position '(-3 "%p"))
;; (setq display-time-default-load-average nil)
;; (setq display-time-load-average nil)
;; (display-time-mode -1) ;; displays current time
;; ;; Default custom modeline
;; (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
;; (setq doom-modeline-modal-icon nil)
;; (setq evil-normal-state-tag   (propertize "[Normal]" )
;;       evil-emacs-state-tag    (propertize "[Emacs]" )
;;       evil-insert-state-tag   (propertize "[Insert]" )
;;       evil-motion-state-tag   (propertize "[Motion]" )
;;       evil-visual-state-tag   (propertize "[Visual]" )
;;       evil-operator-state-tag (propertize "[Operator]" ))

;; (setq evil-insert-state-message nil)
;; (setq evil-visual-state-message nil)
;; (setq evil-replace-state-message nil)
;; (setq evil-motion-state-message nil)
;; (setq hl-line-sticky-flag t)

;;; KEY-CAST
(use-package keycast
  :ensure t
  ;; :after doom-modeline
  :config
  (setopt keycast-mode-line-format
          "%k%r")
  (setopt keycast-substitute-alist
          '((keycast-log-erase-buffer nil nil)
            (transient-update         nil nil)
            (self-insert-command      nil nil)
            (mwheel-scroll nil nil)))
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-modeline)."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(keycast-mode 1)

(use-package diminish
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :config
  (all-the-icons-completion-mode))

(use-package nerd-icons
:ensure t)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; DIRED RELATED
(use-package dired-open
  :ensure t
  :after dired)
  ;; :config
  ;; (setq dired-open-extensions '(
  ;;                               ("jpg" . "imv")
  ;;                               ("png" . "imv")
  ;;                               ;; ("pdf" . "zathura")
  ;;                               ("mkv" . "mpv")
  ;;                               ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(use-package dired-preview
  :ensure t
  :after dired)
(add-hook 'dired-mode-hook #'dired-preview-mode)
(setq dired-preview-delay 0.1)
(setq dired-preview-max-size (expt 2 20))
(setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)"))

(defun my-dired-preview-to-the-right ()
  "My preferred `dired-preview-display-action-alist-function'."
  '((display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4)))

(setq dired-preview-display-action-alist #'my-dired-preview-to-the-right)
;;; EVIL
(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
  evil-want-keybinding nil
  evil-vsplit-window-right t
  evil-split-window-below t
  evil-mode-line-format nil
  evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  :config
  (evil-mode))

(defun my/evil-open-at-point ()
  "Open link at point in org-mode or run `dashboard-return` in dashboard-mode. Do nothing elsewhere."
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (org-open-at-point))

   ((eq major-mode 'dashboard-mode)
    (when (fboundp 'dashboard-return)
      (dashboard-return)))

   (t nil))) ;; do nothing

(define-key evil-normal-state-map (kbd "RET") #'my/evil-open-at-point)

(use-package evil-collection
  :after evil
  :config
      (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
      (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(use-package evil-numbers
  :ensure t
  :after evil)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Using RETURN to follow links in Org/Evil
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package evil-terminal-cursor-changer
  :ensure t)
(setq evil-motion-state-cursor 'box)  ; █
(setq evil-visual-state-cursor 'box)  ; █
(setq evil-normal-state-cursor 'box)  ; █
(setq evil-insert-state-cursor 'hbar)  ; ⎸
(setq evil-emacs-state-cursor  'hbar) ; _

(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode 1)
  ;; Use "jk" to exit insert mode (similar to "jj")
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  ;; Move to the beginning or end of the line with "hh" or "ll"
  (key-chord-define evil-insert-state-map "hh" 'move-beginning-of-line)
  (key-chord-define evil-insert-state-map  "l;" 'move-end-of-line)
  (key-chord-define evil-insert-state-map  "aa" 'move-end-of-line)
  (key-chord-define evil-normal-state-map  "sc" 'evil-avy-goto-char-2)
  (setq key-chord-two-keys-delay 0.5))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-,") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "M-.") 'next-buffer)
  (define-key evil-normal-state-map (kbd "M-q") 'kill-current-buffer))
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;;; COMPANY
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t)

  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

(setq global-company-mode t)
(setq company-tooltip-align-annotations t ; aligns annotation to the right
      company-tooltip-limit 12 ; bigger popup window
      company-idle-delay 0.5   ; decrease delay before autocompletion popup shows
      company-echo-delay (if (display-graphic-p) nil 0)
      company-minimum-prefix-length 2
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-dabbrev-minimum-length 10 ; Use buffer completion at 10 charcters.
      company-global-modes '(not erc-mode message-mode help-mode
                                 gud-mode eshell-mode shell-mode)
      ;;company-backends '((company-capf :with company-yasnippet)
      ;;                   (company-dabbrev-code company-keywords company-files)
      ;;                   company-dabbrev))

      ;; This puts snippet completion first.
      company-backends '((:separate company-yasnippet company-files company-bbdb company-semantic company-cmake company-capf :with company-tempo company-clang
                                    company-dabbrev-code company-gtags company-keywords
                                    company-oddmuse company-dabbrev)))

(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'org-mode-hook #'company-mode)
;;; ORDERLESS
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)))

;;; POSFRAME
(use-package posframe
  :ensure t)

;;; DENOTE
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  ;; Keybinds
  :bind
  (("C-c n n" . denote-create-note)
   ("C-c n l" . denote-link-or-create)
   ("C-c n L" . denote-add-links)
   ("C-c n b" . denote-link-backlinks)
   ("C-c n f" . #'jp:denote-dired-open)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n q c" . denote-query-contents-link)
   ("C-c n q f" . denote-query-filenames-link)
   ("C-c n i i" . denote-insert-image)
   (:map dired-mode-map
         ("C-c C-d C-i" . denote-dired-link-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-files)
         ("C-c C-d C-f" . denote-dired-filter)
         ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))))
(add-hook 'dired-mode-hook #'denote-dired-mode)


(setq denote-directory (expand-file-name "~/docs/notes"))
(setq denote-known-keywords '("estudio" "trabajo" "emacs" "linux"))
(setq denote-title-history nil)
(setq denote-sort-keywords nil)
(setq denote-files-matching-regexp-history nil)
(setq denote-history-completion-in-prompts nil)
(setq denote-infer-keywords t)
(setq denote-org-front-matter "# -*- jinx-languages: \"es_ES\"; -*-\n#+title: %s\n#+date: %s\n#+filetags: %s\n#+identifier: %s\n#+author: Ing. Javier Pacheco\n#+startup: showall\n\n")
(setq denote-query-links-display-buffer-action
      '((display-buffer-same-window)))
(setq denote-link--prepare-links-format "%s\n")

(defun jp:denote-dired-open()
  "Open `denote-directory` in Dired and filter only notes matching proper Denote filename pattern."
  (interactive)
  (dired denote-directory)
  ;; (dired-mark-files-regexp "^[0-9]\\{8\\}T[0-9]\\{6\\}--[^=].*\\.org$")
  ;; (dired-toggle-marks)
  ;; (dired-do-kill-lines)
  (let ((messages '(
                    "🧠🌿 Mind garden pruned: only pure ideas are blooming."
                    "🧠✨ Mind map refreshed: only clear branches remain."
                    "🪴📝 Notes pruned: a clean path through your thoughts."
                    "🔍🌟 Only well-formed thoughts sparkle here now."
                    "📜🌱 Organized scrolls: the chaos fades, clarity grows."
                    "🧩📚 Puzzle pieces placed: only true notes stay."
                    "🌌✍️ Mental constellation aligned: shining ideas ahead."
                    "🌿📖 Your knowledge forest breathes freely now."
                    )))
    (message "%s" (nth (random (length messages)) messages))))

(defun my/denote-or-org-link ()
  "Run `denote-find-link`. If nothing is inserted, show Org links in minibuffer."
  (interactive)
  (let ((before (buffer-substring-no-properties (point-min) (point-max))))
    (call-interactively #'denote-find-link)
    (run-at-time
     "0.1 sec" nil
     (lambda ()
       (let ((after (buffer-substring-no-properties (point-min) (point-max))))
         (when (string= before after)
           (let ((links (my/org-collect-links)))
             (if links
                 (let ((chosen (completing-read "Org links: " links)))
                   (when chosen
                     (kill-new chosen)
                     (message "Copied link to clipboard: %s" chosen)))
               (message "No Denote or Org links found.")))))))))

(defun my/org-collect-links ()
  "Collect all Org-style links ([[...]]) in the current buffer."
  (let (links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (push (match-string 0) links)))
    (delete-dups links)))

(defun denote-insert-image ()
  "Prompt to select an image from ~/docs/notes/img/ and insert its absolute path as [[...]] link."
  (interactive)
  (let* ((img-dir (expand-file-name "~/docs/notes/img/"))
         (filename (read-file-name "Select image: " img-dir nil t)))
    (when (and filename (file-exists-p filename))
      (insert (format "[[%s]]" (expand-file-name filename))))))

(defun denote-dired-filter ()
  "Mark files by regex, toggle marks, and kill selected files.
Follows the sequence: % m (regex), t, K."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (message "Enter regex to mark files: ")
  (call-interactively 'dired-mark-files-regexp) ; % m (user enters regex)
  (dired-toggle-marks)                         ; t (toggle marks)
  (dired-do-kill-lines))                       ; K (kill marked files)

(defun denote-insert-pdf-link ()
  "Insert a Denote-style Org link to a PDF file from ~/docs/pdf, prompting for a custom link title."
  (interactive)
  (let* ((pdf-dir (expand-file-name "~/docs/pdf"))
         (pdf-files (directory-files-recursively pdf-dir "\\.pdf$"))
         (chosen-file (completing-read "Choose a PDF: " pdf-files nil t))
         (custom-title (read-string "Enter a link title: ")))
    (insert (format "- [[file:%s][%s]]"
                    (file-relative-name chosen-file)
                    custom-title))))

(use-package consult-denote
  :ensure t)

(use-package denote-silo
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/docs/notes/inbox/")))

(use-package denote-sequence
  :ensure t
  :bind
  ( :map global-map
    ("C-c n s s" . denote-sequence-new-sibling)
    ("C-c n s p" . denote-sequence-new-parent)
    ("C-c n s f" . denote-sequence-find)
    ("C-c n s l" . denote-sequence-link)
    ("C-c n s d" . denote-sequence-dired)
    ("C-c n s r" . denote-sequence-reparent)
    ("C-c n s c" . denote-sequence-new-child))
  :config
  (setq denote-sequence-scheme 'numeric)
  (setq denote-sequence-type-history nil))

(use-package denote-menu
  :ensure t
  :custom ((denote-menu-show-file-type nil)
           (denote-menu-initial-regex "==[0-9]+--.*_meta.org"))
  :bind
  (("C-c n m" . denote-menu-list-notes)))

(use-package denote-explore
  :ensure t
  :bind* (("C-c e n" . denote-explore-network)
        ("C-c e v" . denote-explore-network-regenerate)
        ("C-c e D" . denote-explore-barchart-degree)))
(setq denote-explore-network-d3-template "~/.emacs.d/explore.html")


;;; GIT
(use-package undohist
  :ensure t)
(undohist-initialize)

(use-package git-timemachine
  :defer t
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package magit
  :ensure t)
;; Use this windows normy PATH if using windows ...
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files/Git/bin"))

;; (use-package git-gutter
;;   :init (global-git-gutter-mode 1)
;;   :config
;;   (setq git-gutter:update-interval 0.02)
;;   (set-face-background 'git-gutter:added nil)
;;   (set-face-background 'git-gutter:modified nil)
;;   (set-face-background 'git-gutter:deleted nil))

;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package transient
  :defer t)

;;; KEYBINDS AND KEYS
(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
    which-key-sort-order #'which-key-key-order-alpha
    which-key-allow-imprecise-window-fit nil
    which-key-sort-uppercase-first nil
    which-key-add-column-padding 1
    which-key-max-display-columns nil
    which-key-min-display-lines 6
    which-key-side-window-slot -10
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.8
    which-key-max-description-length 25
    which-key-allow-imprecise-window-fit nil
    which-key-separator " → " ))

(use-package general
  :config
  (general-evil-setup)
  (eval-after-load "org" '(define-key org-mode-map (kbd "C-j") nil))
  (eval-after-load "org" '(define-key org-mode-map (kbd "C-k") nil))
  (eval-after-load "org" '(define-key org-mode-map (kbd "M-l") nil))
  (general-define-key
   :states '(normal insert motion)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "M-j" 'move-line-down
   "M-k" 'move-line-up
   "C-l" 'evil-window-right
   "M-l" 'org-make-olist)
  (general-define-key
   :states '(normal)
   "SPC SPC" 'my/org-edit-toggle
   ;; "SPC SPC" 'hydra-of-hydras/body
   "g V" 'cycle-region-preview
   "C-;" 'devdocs-lookup
   "C-\\" 'ispell-comment-or-string-at-point
   "<backspace>" 'org-mark-ring-goto
   ;; "K" 'eldoc-box-help-at-point)
   "K" 'my-show-doc-or-describe-symbol)
  ;; "K" 'describe-symbol-at-point)

  (general-define-key
   :states '(normal visual)
   "<" "<gv"
   ">" ">gv"
   )
  (define-key evil-insert-state-map (kbd "C-c C-c") 'evil-normal-state)

  (define-key evil-visual-state-map (kbd "<") (lambda ()
                                                (interactive)
                                                (evil-shift-left (region-beginning) (region-end))
                                                (evil-normal-state)
                                                (evil-visual-restore)))
  (define-key evil-visual-state-map (kbd ">") (lambda ()
                                                (interactive)
                                                (evil-shift-right (region-beginning) (region-end))
                                                (evil-normal-state)
                                                (evil-visual-restore)))

  ;; set up 'SPC' as the global leader key
  (general-create-definer user/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-SPC") ;; access leader in insert mode

  (user/leader-keys
    "." '(find-file :wk "Find file")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

  (evil-define-key '(normal visual) 'global
    (kbd "g c c") #'evilnc-comment-or-uncomment-lines)

  (evil-define-key '(visual) 'global
    (kbd "g c") #'evilnc-comment-or-uncomment-lines)

  (user/leader-keys
    "a" '(:ignore t :wk "Ellama A.I.")
    "a a" '(ellama-ask-about :wk "Ask ellama about region")
    "a e" '(:ignore t :wk "Ellama enhance")
    "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
    "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
    "a c a" '(ellama-code-add :wk "Ellama  add")
    "a c i" '(ellama-code-improve :wk "Ellama  improve")
    "a i" '(ellama-chat :wk "Ask ellama")
    "a p" '(ellama-provider-select :wk "Ellama provider select")
    "a s" '(ellama-summarize :wk "Ellama summarize region")
    "a t" '(ellama-translate :wk "Ellama translate region"))

  ;; "a" '(:ignore t :wk "Agenda buffers")
  ;; "a" '(org-agenda :wk "Open the agenda")

  (user/leader-keys
    "j" '(:ignore t :wk "Agenda buffers")
    "j l" '(avy-goto-line :wk "Avy go to line")
    "j w" '(avy-goto-char-2 :wk "Avy go to word"))

  (user/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(consult-buffer :wk "Switch to buffer")
    "b c" '(ispell-buffer :wk "Buffer spell-checking")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(consult-org-heading :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (user/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired"))

  (user/leader-keys
    "e" '(:ignore t :wk "Eshell/Eval/EWW")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(open-specific-dired :wk "Edit Configuration Files")
    "e o" '(open-org-files :wk "Open my docs org files")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(term-toggle-eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (user/leader-keys
    "f" '(:ignore t :wk "Files")
    "f e" '((lambda () (interactive)
              (dired "~/.emacs.d"))
            :wk "Open user-emacs-directory in dired")
    "f b" '((lambda () (interactive)
              (find-file "~/webdev/jpachecoxyz.github.io/hugo/org/jpacheco.xyz.org"))
            :wk "Open web org file.")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f f" '(my/transient-goto-file-buffer :wk "Go to buffer or file.")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f l" '(denote-find-link :wk "Denote find links")
    "f p" '((lambda () (interactive)
              (find-file "~/.emacs.d/init.el"))
            :wk "Open noobemacs Configuraiton file.")
    "f r" '(recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f n" '(consult-denote-find :wk "Find Denotes")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (user/leader-keys
    "g" '(:ignore t :wk "Git")
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create")
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g d" '(magit-diff-buffer-file :wk "Open a diff file in a new buffer")
    "g f" '(:ignore t :wk "Find")
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

  (user/leader-keys
    "h" '(:ignore t :wk "Help")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.emacs.d/init.el"))
              :wk "Reload emacs config")
    "h t" '(fz-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (user/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (user/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (user/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (user/leader-keys
    "o" '(:ignore t :wk "Open")
    ;; "o -" '(ee-nnn :wk "Dired jump to current")
    "o -" '(dired-jump :wk "Dired jump to current")
    "o o" '(hydra-agenda-files/body :wk "Open org-agenda files")
    "o a" '( (lambda () (interactive) (org-agenda nil "a")) :w "Open org Agenda")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o i" '(jp/org-id-store-link-for-headers :wk "Add ID's to org headers.")
    "o l" '(open-lisp-and-org-files :wk "Open lisp files")
    "o m" '((lambda () (interactive)
              (find-file "~/docs/notes/20250327T190743--mimir__meta.org"))
            :wk "Open web org file.")
    "o p" '((lambda () (interactive)
              (find-file "~/webdev/jpachecoxyz/org/jpacheco.xyz.org"))
            :wk "Open web org file.")
    "o L" '(list-and-open-url-in-buffer :wk "Follow urls in buffer")
    "o t" '(toggle-eshell-buffer :wk "Toggle terminal")
    "o s" '(toggle-scratch-buffer :wk "Toggle scratch buffer")
    "o e" '(toggle-org-buffer :wk "Toggle org buffer")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (user/leader-keys
    "p" '(:ignore t :wk "Projectile")
    "p a" '(projectile-add-known-project :wk "Add a project directory"))

  (user/leader-keys
    "r" '(:ignore t :wk "Denote")
    "r f" '(denote-open-or-create :wk "Denote open note")
    "r u" '(denote-explore-network :wk "Open denote explorer")
    "r m" '(denote-menu-list-notes :wk "Denote-menu")
    "r i" '(denote-insert-link :wk "Insert denote link")
    "r I" '(jp:denote-update-links-matching-regexp :wk "Update and insert denote missing links")
    )

  (user/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s c" '(denote-sequence-new-child-of-current :wk "Denote child of current")
    "s s" '(denote-sequence-new-sibling-of-current :wk "Denote sibling of current")
    "s p" '(denote-sequence-new-parent :wk "Denote new parent")
    "s m" '(man :wk "Man pages")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (user/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(jp/org-toggle-emphasis-markers :wk "Toggle org-emphasis")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t o" '(org-mode :wk "Toggle org mode")
    "t p" 'org-export-to-latex-and-compile-with-tectonic :wk "Export this buffer to pdf using Tectonic"
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  (user/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))

  (user/leader-keys
    "q" '(:ignore t :wk "Quit Emacs")
    ;; Quiting Emacs Options
    "q r" '(restart-emacs :wk "Restart Emacs")
    "q q" '(kill-emacs :wk "Exit Emacs"))
  )

;;; CONSULT
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-c o" . consult-outline)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; (setq register-preview-delay 0.5
  ;;       register-preview-function #'consult-register-format)
  ;; (advice-add #'register-preview :override #'consult-register-window)
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; "C-+"
)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flycheck
  :ensure t)

;; VERTICO
(use-package savehist
  :config
    (setq history-length 25)
    (savehist-mode 1))

(defun dw/minibuffer-backward-kill (arg)

  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#5C6370"))))
  :init
  (vertico-mode))
  ;; (vertico-posframe-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico-posframe
  :ensure t)

(add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 20)))
(vertico-multiform-mode 1)

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annonators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;; BREAK LINES
(use-package form-feed
  :config (global-form-feed-mode))

(use-package page-break-lines
  :config
  (page-break-lines-mode))


;;; CODING RELATED
(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :hook ((org-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))
(add-hook 'org-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(use-package highlight-indent-guides
  :config
    (setq highlight-indent-guides-method 'bitmap)
    (setq highlight-indent-guides-auto-enabled nil)

    (set-face-background 'highlight-indent-guides-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-character-face "#458588")
   :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package highlight-thing
  :ensure t
  :hook (prog-mode . highlight-thing-mode))

(setq highlight-thing-delay-seconds 0.2)
(setq highlight-thing-exclude-thing-under-point nil)

(use-package yasnippet
  ;; :defer 2
  ;; :init (yas-reload-all)
  :custom (yas-keymap-disable-hook (lambda () (frame-visible-p corfu--frame)))
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

(use-package yasnippet-snippets
  :ensure t)

(use-package ivy-yasnippet
  :ensure t)

(use-package fill-column-indicator
  :ensure nil
  :config
    (set-face-background 'fill-column-indicator "white"))

; Settings:
(setq-default fill-column 80)
;; Enable display-fill-column-indicator
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'org-mode-hook #'display-fill-column-indicator-mode)

(defun my-show-doc-or-describe-symbol ()
  "Show LSP UI doc if LSP is active, otherwise describe symbol at point."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-doc-glance)
    (describe-symbol-at-point)))

(defun describe-symbol-at-point ()
  "Display the documentation of the symbol at point, if it exists."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol
        (cond
         ((fboundp symbol) (describe-function symbol))
         ((boundp symbol) (describe-variable symbol))
         (t (message "No documentation found for symbol at point: %s" symbol)))
      (message "No symbol at point"))))

;;; ORG
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))
(add-hook 'org-mode-hook #'org-auto-tangle-mode)

(use-package org-mime
  :ensure t)

(setq org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil))

(use-package org-sidebar
  :ensure t)

(use-package ox-hugo
  :ensure t
  :after ox)
(setq org-hugo-base-dir "~/webdev/jpachecoxyz/")
(defun jp:create-hugo-post ()
  "Create a new Hugo post buffer with metadata in Org format, unsaved."
  (interactive)
  (let* ((title (read-string "Post title: "))
         (description (read-string "Post description: "))
         (tags (read-string "Tags (separated by spaces): "))
         (is-draft (y-or-n-p "Is this a draft? "))
         (slug (replace-regexp-in-string " " "-" (downcase title)))
         (file-name (concat slug ".org"))
         (file-path (expand-file-name file-name "~/webdev/jpachecoxyz/org/posts/"))
         (date (format-time-string "%Y-%m-%d"))
         (draft-string (if is-draft "true" "false"))) ;; <-- move the IF here!
    (find-file file-path)
    (insert (format "#+title: %s\n" title))
    (insert (format "#+description: %s\n" description))
    (insert (format "#+date: %s\n" date))
    (insert (format "#+export_file_name: %s\n" slug))
    (insert "#+hugo_base_dir: ~/webdev/jpachecoxyz/\n")
    (insert "#+hugo_section: posts\n")
    (insert (format "#+hugo_tags: %s\n" tags))
    (insert "#+hugo_custom_front_matter: toc true\n")
    (insert "#+hugo_auto_set_lastmod: nil\n")
    (insert (format "#+hugo_draft: %s\n" draft-string)) ;; <- use the precomputed value here
    (goto-char (point-max))
    (insert "\n") ;; Ensure a blank line before the cursor
    (set-buffer-modified-p t)))

(global-set-key (kbd "C-c n p") #'jp:create-hugo-post)

(use-package org-rainbow-tags
  :ensure t
  :custom
  (org-rainbow-tags-hash-start-index 20)
  (org-rainbow-tags-extra-face-attributes
   '(:inverse-video nil :box nil :weight 'bold))
  :hook
  (org-mode . org-rainbow-tags-mode))
(add-hook 'org-mode-hook #'org-rainbow-tags-mode)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(require 'org-indent)
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("⁖" "⁖" "⁖" "⁖" "⁖" "○" "●")))
  ;; (org-bullets-bullet-list '("" "" "" "" "" "" "")))
(add-hook 'org-mode-hook #'org-bullets-mode)

(use-package org-download
  :ensure t
  :defer t)

(require 'org-tempo)
(require 'org-id)
(setq org-id-link-to-org-use-id 'use-existing)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-x x i") 'jp/org-id-headline)
(global-set-key (kbd "C-x x I") 'jp/org-id-headlines)

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'isearch)
  (pulsar-global-mode 1)
  :bind ("<f2>" . pulsar-pulse-line))

(defun terror/slide-setup ()
  (global-hl-line-mode -1)
  (org-bullets-mode 1)
  (setq text-scale-mode-amount 2)
  (text-scale-mode 1)
  (emacs-solo/center-document-mode 1)
  (set-frame-parameter (selected-frame)
                       'internal-border-width 50)
  (org-display-inline-images)
  (toggle-frame-fullscreen)
  (hide-mode-line-mode 1)
  (hide-lines-matching "#\\+begin_src")
  (hide-lines-matching "#\\+end_src"))

(defun terror/slide-end ()
  (global-hl-line-mode -1)
  (setq text-scale-mode-amount 0)
  (text-scale-mode -1)
  (emacs-solo/center-document-mode -1)
  (set-frame-parameter (selected-frame)
                       'internal-border-width 0)
  (toggle-frame-fullscreen)
  (hide-mode-line-mode -1)
  (org-fold-show-all))

(use-package org-tree-slide
  :ensure t
  :after org
  :hook ((org-tree-slide-play . terror/slide-setup)
         (org-tree-slide-stop . terror/slide-end))
  :init
  (setq org-image-actual-width nil
        org-tree-slide-header t
        org-tree-slide-breadcrumbs " > "
        org-tree-slide-activate-message "Presentation Begins"
        org-tree-slide-deactivate-message "End of presentation"))

(add-hook 'org-tree-slide-play-hook #'terror/slide-setup)
(add-hook 'org-tree-slide-stop-hook #'terror/slide-end)


(global-set-key (kbd "<f12>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f12>") 'org-tree-slide-skip-done-toggle)
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f1>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f2>") 'org-tree-slide-move-next-tree))

; Settings
(global-visual-line-mode t)  ;; Enable truncated lines
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (awk . t)
   ))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-startup-folded 'overview)
(setq org-adapt-indentation nil)
(setq org-support-shift-select t)
(setq org-log-done 'time)
(setq org-hide-emphasis-markers t)
(setq org-log-into-drawer t)
(setq org-ellipsis " ⤵")
(setq org-directory "~/public/org/")
(setq org-tag-alist
      '(;;Places
        ("@home" . ?h)
        ("@work" . ?W)

        ;; Whom
        ("lia" . ?l)
        ("jr" . ?j)
        ("xiomara" . ?x)

        ;; Devices
        ("@laptop" . ?L)

        ;; Activities
        ("programming" . ?p)
        ("planning" . ?n)
        ("writting" . ?w)
        ("email" . ?e)
        ("crypt" . ?c)
        ))
(setq org-agenda-files
      '(
        ;; "~/public/org/agenda/personal.org"
        ;; "~/public/org/agenda/training.org"
        "~/docs/org/agenda/bdays.org"
        "~/docs/org/agenda/important_dates.org"
        ;; "~/public/org/agenda/contacts.org"
        ;; "~/public/org/agenda/work.org"
        "~/docs/org/agenda/agenda.org"
        ))
(setq org-todo-keywords
    (quote ((sequence "TODO" "DOING" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
;; TODO colors
(setq org-todo-keyword-faces
    '(
    ("TODO" . (:foreground "#d65d0e" :weight italic))
    ("DOING" . (:foreground "#458588" :weight italic))
    ("WAITING" . (:foreground "#98971a" :weight italic))
    ("HOLD" . (:foreground "#d79921" :weight italic))
    ("DONE" . (:foreground "#689d6a" :weight italic))
    ("CANCELLED" . (:foreground "#9d0006" :weight italic))))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Attempt to open info files in new windows.
(setq org-link-frame-setup
      '((file . find-file)))

(defun jp-autorefile-tasks ()
  "Runs org-refile when the task state changes to HOLD."
  (interactive)
  (when (and (string= (org-get-todo-state) "HOLD")
             (eq this-command 'org-todo))
    (org-refile)))

(add-hook 'org-after-todo-state-change-hook 'jp-autorefile-tasks)

(setq org-structure-template-alist
      '(("ss" . "src")
        ("se" . "src emacs-lisp")
        ("st" . "src emacs-lisp :tangle FILENAME")
        ("sT" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
        ("sx" . "src shell :tangle FILENAME")
        ("sX" . "src shell :tangle FILENAME :shebang \"#!/usr/bin/env bash\"")
        ("e" . "example")
        ("X" . "export")))

;;; ORG-AGENDA
(defvar custom-daily-agenda
  `(
    (tags-todo "+@home|@work"
               ((org-agenda-span 'week)
                (org-agenda-start-on-weekday 1) ; Start the week on Monday
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Main Agenda Overview\n")))

    ;; (tags-todo "*"
    ;;            ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
    ;;             (org-agenda-skip-function
    ;;              `(org-agenda-skip-entry-if
    ;;                'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-overriding-header "Important Tasks\n")))
    (agenda "" ((org-agenda-span 0)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 3)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
    ;; (agenda "" ((org-agenda-start-on-weekday nil)
    ;;             (org-agenda-start-day "+1d")
    ;;             (org-agenda-span 5)
    ;;             (org-deadline-warning-days 0)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
    ;;             ;; (org-agenda-entry-types '(:deadline))
    ;;             (org-agenda-overriding-header "\nNext five days\n")))
(agenda "" ((org-agenda-time-grid nil)
            (org-agenda-start-on-weekday nil)
            ;; We don't want to replicate the previous section's
            ;; three days, so we start counting from the day after.
            (org-agenda-start-day "+6d")
            (org-agenda-span 14)
            (org-agenda-show-all-dates nil)
            (org-deadline-warning-days 0)
            (org-agenda-block-separator nil)
            (org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            (org-agenda-overriding-header "\nUpcoming tasks (+14d)\n")))
)
"Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-agenda-custom-commands
      `(
        ("a" "Daily agenda and top priority tasks"
         ,custom-daily-agenda
         ((org-agenda-fontify-priorities nil)
          (org-agenda-dim-blocked-tasks nil)))

        ;; ("p" "Personal Agenda"
        ;;  ,custom-daily-agenda
        ;;  ((org-agenda-files '("~/public/org/agenda/personal.org"
        ;;                    "~/public/org/agenda/training.org"))
        ;;   (org-agenda-fontify-priorities nil)
        ;;   (org-agenda-dim-blocked-tasks nil)))

        ("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-overriding-header "Completed Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                   (org-agenda-span 'week)))

          (agenda ""
                  ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-span 'week)))))

        ("W" "Work Agenda"
         ,custom-daily-agenda
         ((org-agenda-files '("~/public/org/agenda/work.org")
                            (org-agenda-fontify-priorities nil)
                            (org-agenda-dim-blocked-tasks nil))))

        ("p" "Planning"
         ((tags-todo "+planning+@home|@work"
                     ((org-agenda-overriding-header "Planning Tasks")))

          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "Untagged Tasks")))

          (todo ".*" ((org-agenda-files '("~/public/org/agenda/refill.org"))
                      (org-agenda-overriding-header "Unprocessed refill.org Items")))))

        ("i" "Important dates"
         ((agenda ""
                  ((org-agenda-overriding-header "Important dates Agenda Overview\n")
                   (org-agenda-span 'year)
                   (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                   (org-agenda-show-all-dates nil)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if
                      'notregexp
                      (regexp-opt '("i-dates"))))))

          (agenda ""
                  ((org-agenda-overriding-header "Upcoming Birthday's\n")
                   (org-agenda-span 'month)
                   (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                   (org-agenda-start-day "01")
                   (org-agenda-show-all-dates nil)
                   (org-agenda-files '("~/public/org/agenda/bdays.org"))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if
                      'notregexp
                      (regexp-opt '("birthday"))))))))

        ("b" "Birthday Calendar dates"
         ((agenda ""
                  ((org-agenda-overriding-header "Birthday Calendar dates\n")
                   (org-agenda-span 'year)
                   (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                   (org-agenda-start-day "01")
                   (org-agenda-show-all-dates nil)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if
                      'notregexp
                      (regexp-opt '("birthday"))))))))

        ))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map (kbd "<tab>") 'org-agenda-next-item)))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map (kbd "<backtab>") 'org-agenda-previous-item)))

(add-hook 'org-agenda-mode-hook 'page-break-lines-mode)
(global-set-key (kbd "C-c C-h") 'consult-org-agenda)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-track-ordered-property-with-tag t)
(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)

;;; ORG-CAPTURE
(setq org-default-notes-file '("~/public/org/agenda/refill.org"))
(global-set-key (kbd "C-c c") 'org-capture)      ;; use C-c c to start capture mode

;; capture templates for: TODO tasks, Notes, appointments, meetings
(setq org-templates-location-var (concat org-directory "agenda/refill.org"))

(defun jpacheco/org-capture-new-post ()
  (let ((filename (read-string "Enter the name of the file (without extension): ")))
    (expand-file-name (concat filename ".org") "~/repos/jpacheco.xyz/content/posts/")))

(setq org-capture-templates
      `(
        ("s" "Scheduled Task" entry (file+headline "~/public/org/agenda/refill.org" "Priority")
         "** TODO [#A] %? %^G \n  SCHEDULED: %^t" :empty-lines 1)

        ("d" "Deadline" entry (file+headline "~/public/org/agenda/refill.org" "Deadline")
         "** TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)

        ("n" "Note" entry (file+headline "~/public/org/agenda/refill.org" "Notes")
         "** %? %^G\n" :empty-lines 1)

        ("c" "Add contact" entry (file+headline "~/public/org/agenda/contacts.org" "Familia")
         my/org-contacts-template
         :empty-lines 1)
        ))

;; Refile
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; C-c C-w for refile
;; (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 1))))

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for a new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: "))          ; Prompt for the post title
           (fname (org-hugo-slug title))                          ; Generate a slug for the filename
           (description (read-from-minibuffer "Description: "))   ; Prompt for the post description
           (org-buffer (current-buffer)))                          ; Get the current buffer

      (mapconcat #'identity
                 `(
                   ,(concat "** TODO " title)                     ; Headline with the TODO and title
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DESCRIPTION: " description)
                   ,(concat ":DATE: " (format-time-string "[%Y-%m-%d %a]"))
                   ":EXPORT_HUGO_SECTION: posts"
                   ":END:"
                   ""
                   "*** %?\n")                                   ; Place the cursor here finally
                 "\n"))))

;;; CUSTOM ELISP CODE
;; Open files in the lisp folder
(require 'find-lisp)
(defun open-lisp-and-org-files ()
  "Open a Lisp or Org file from ~/.emacs.d/lisp directory, including subfolders."
  (interactive)
  (let* ((directory "~/.emacs.d/lisp")
         (el-files (find-lisp-find-files directory ".*\\.el$"))
         (org-files (find-lisp-find-files directory ".*\\.org$"))
         (all-files (append el-files org-files))
         (file (completing-read "Select file: " all-files nil t)))
    (find-file file)))

(defun open-org-files ()
  "Open a Lisp or Org file from my docs directory, including subfolders."
  (interactive)
  (let* ((directory "~/docs/org")
         (org-files (find-lisp-find-files directory ".*\\.org$"))
         (all-files (append org-files))
         (file (completing-read "Select file: " all-files nil t)))
    (find-file file)))

;; Follow urls in the buffer
(defun list-and-open-url-in-buffer ()
  "List all URLs in the current buffer, display them in the minibuffer, and open a selected URL in the browser."
  (interactive)
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(http\\|https\\|ftp\\|file\\|mailto\\):[^ \t\n]+" nil t)
        (push (match-string 0) urls)))
    (if urls
        (let ((url (completing-read "Select URL to open: " (reverse urls) nil t)))
          (browse-url url))
      (message "No URLs found in the buffer."))))

;; Export org files to pdf using tectonic
(defun org-export-to-latex-and-compile-with-tectonic ()
  "Export the current Org file to LaTeX, compile with tectonic using shell-escape,
   delete the .tex file, and move the generated PDF to the pdf/ directory."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (tex-file (concat (file-name-sans-extension org-file) ".tex"))
         (pdf-file (concat (file-name-sans-extension org-file) ".pdf"))
         (tectonic-command (concat "tectonic -Z shell-escape " tex-file))
         (pdf-dir "~/docs/pdf/"))
    ;; Export Org file to LaTeX
    (org-latex-export-to-latex)
    ;; Run tectonic command in a temporary buffer to avoid displaying the output
    (with-temp-buffer
      (shell-command tectonic-command (current-buffer)))
    ;; Check if the PDF was successfully generated
    (if (file-exists-p pdf-file)
        (progn
          ;; Delete the .tex file
          (delete-file tex-file)
          ;; Create pdf/ directory if it doesn't exist
          (unless (file-directory-p pdf-dir)
            (make-directory pdf-dir))
          ;; Move the PDF to the pdf/ directory
          (rename-file pdf-file (concat pdf-dir (file-name-nondirectory pdf-file)) t)
          (message "Compiled %s to PDF and moved to pdf folder." tex-file))
      (message "PDF generation failed."))))

;; (global-set-key (kbd "C-c e l") 'org-export-to-latex-and-compile-with-tectonic)

;; Update my web-page
(defun publish-my-blog ()
  "Export all subtrees with Hugo, then run the publish blog script within Emacs and display a success message in the minibuffer."
  (interactive)
  (let ((commit-msg (read-string "Enter commit message: ")))
    ;; Export all subtrees with Hugo
    (org-hugo-export-wim-to-md :all-subtrees)
    ;; Run the publish script
    (let ((process (start-process-shell-command
                    "publish-blog"                       ; Process name
                    "*publish-blog-output*"              ; Output buffer
                    (format "~/webdev/jpachecoxyz/publish.sh \"%s\"" commit-msg))))  ; Run the script with the commit message
      ;; Set up the process sentinel to check the process status
      (set-process-sentinel
       process
       (lambda (process event)
         (when (string= event "finished\n")
           (message "jpacheco.xyz was correctly updated!")))))))

(global-set-key (kbd "C-c u b") 'publish-my-blog)

(defun jp/yt-shorts-timer ()
  (interactive)
  (org-timer-set-timer "55s"))
(global-set-key (kbd "<f4>") 'jp/yt-shorts-timer)

;; A function to toggle between org-edit-special and org-edit-src-exit
(defun my/org-edit-toggle ()
  "Toggle between org-edit-special and org-edit-src-exit."
  (interactive)
  (if (org-src-edit-buffer-p)  ; Check if we're in the special edit buffer
      (org-edit-src-exit)      ; If inside the edit buffer, exit
    (if (org-in-src-block-p)   ; Check if we're in a source block in org-mode
        (org-edit-special)     ; If in a source block, edit it
      (message "Not in a source block.")))) ; If not, do nothing

;; open dired in to especific directories
(defun open-specific-dired ()
  "Ask whether to open config, scripts, or nix config in Dired."
  (interactive)
  (let ((choice (completing-read "Choose an option: " '("config" "scripts" "docs"))))
    (cond
     ((string= choice "config")
      (fzf-find-file "~/.config/"))
     ((string= choice "scripts")
      (fzf-find-file "~/.local/bin/"))
     ((string= choice "docs")
      (fzf-find-file "~/docs/org/"))
     (t
      (message "Invalid choice")))))

;; Insert block code:
(defun org-mode-insert-code (language results export)
  "Insert a code block in Org mode with specified LANGUAGE, RESULTS, and EXPORT options.
   Place the cursor inside and switch to insert mode (for evil-mode users)."
  (interactive
   (list
    (read-string "Language: ")        ;; Prompt for language
    (read-string "Results: " "output") ;; Prompt for results (default 'output')
    (read-string "Export: " "both")))  ;; Prompt for export (default 'both')
  ;; Insert the code block with language, results, and exports options
  (insert (format "#+BEGIN_SRC %s :results %s :exports %s\n\n#+END_SRC\n"
                  language results export))
  ;; Move the cursor to the middle line (inside the code block)
  (forward-line -2)
  ;; Switch to insert mode if evil-mode is enabled
  (when (bound-and-true-p evil-mode)
    (evil-insert-state)))
(global-set-key (kbd "C-c i c") #'org-mode-insert-code)

;; Yank the content of a src org block.
(defun yank-org-src-block-content ()
  "Yank the content of the source block at point."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((element (org-element-at-point))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element)))
      ;; Move point to the content of the block and copy it to the kill-ring
      (save-excursion
        (goto-char begin)
        (re-search-forward "^[ \t]*#\\+begin_src[^\n]*\n" end t)
        (let ((content-start (point)))
          (re-search-forward "^[ \t]*#\\+end_src" end t)
          (kill-ring-save content-start (match-beginning 0))))
      (message "Yanked source block content!"))))
(global-set-key (kbd "C-c i y") #'yank-org-src-block-content)


(prog1 'my/transient-goto-file-buffer
  ;; List
  (setq my/goto-file-buffer-alist
        '(("s" "*scratch*"    (switch-to-buffer "*scratch*"))
          ("h" "home.nix"       (find-file "~/.dotfiles/nix/home.nix"))
          ("c" "configuration.nix"       (find-file "~/.dotfiles/nix/configuration.nix"))
          ("u" "utilities.org"       (find-file "~/.emacs.d/lisp/utilities.org"))
          ("j" "Journal"      (find-file (expand-file-name "agenda/journal.org" org-directory)))
          ("n" "Notes"        (find-file (expand-file-name "agenda/notes.org" org-directory)))))
  ;; Command
  (defun my/goto-file-buffer ()
    "Jump to a file or buffer based on the key used to invoke this command."
    (interactive)
    (let* ((key (this-command-keys))
           (choice (assoc key my/goto-file-buffer-alist))
           (form (caddr choice)))
      (eval form)))
  ;; Transient
  (eval
   `(transient-define-prefix my/transient-goto-file-buffer ()
      [,(apply 'vector
               "Goto Files & Buffers"
               (mapcar (lambda (x) (list (car x) (cadr x) 'my/goto-file-buffer))
                       my/goto-file-buffer-alist))])))

;;; WINDOW RULES
(use-package shackle
  :custom
  ((shackle-rules
    (let ((repls "\\*\\(cider-repl\\|sly-mrepl\\|ielm\\)")
          (vcs   "\\*\\(Flymake\\|Package-Lint\\|vc-\\(git\\|got\\) :\\).*")
          (docs "\\*devdocs\\*")
          (roam "\\*Capture\\*")
          (org-log "\\*Org Note\\*")
          (warnings "\\*Warnings\\*")
          (magit "Magit")
          (ellama "\\(.*(zephyr)\\.org\\)")
          ;; (dired "Dired by name")
          (scratch    "\\*scratch\\*"))
      `((compilation-mode :noselect t
                          :align above
                          :size 0.2)
        ("*Async Shell Command*" :ignore t)
        (,repls :regexp t
                :align below
                :size 0.3)
        (,org-log :regexp t
                  :align below
                  :size 0.3)
        (occur-mode :select t
                    :align right
                    :size 0.3)
        (diff-mode :select t)
        (,docs :regexp t
               :size 0.4
               :align right
               :select t)
        (,ellama :regexp t
                 :same t)
        (,warnings :regexp t
                   :ignore t)
        (help-mode :select t
                   :align below
                   :size 0.5)
        (,vcs :regexp t
              :align above
              :size 0.15
              :select t)
        (,scratch :regexp t
                  :same t
                  :select t)
        (,magit :regexp t
                :same t
                :select t)
        (,roam :regexp t
               :same t
               :select t))))
   (shackle-default-rule nil ; '(:inhibit-window-quit t)
                         ))
  :config (shackle-mode))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)) ;; Optional keybindings
  :custom
  (popper-reference-buffers
   '("\\*Async Shell Command\\*"
     help-mode
     compilation-mode
     "\\(.*(zephyr)\\.org\\)"
     "\\*Warnings\\*"
     "\\*Messages\\*"))
  (popper-display-control t) ;; Let Popper handle popups entirely
  :config
  (popper-mode +1)
  (popper-echo-mode +1)) ;; For echo messages when popups are toggled

;;; FZF
(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq
   fzf/args
   "--color=fg:-1,fg+:#d0d0d0,bg:-1,bg+:#282828 --color=hl:#5f87af,hl+:#5fd7ff,info:#afaf87,marker:#87ff00 --color=prompt:#458588,spinner:#af5fff,pointer:#af5fff,header:#87afaf --color=gutter:-1,border:#262626,label:#aeaeae,query:#d9d9d9 --border='bold' --border-label='' --preview-window='border-bold' --prompt='❯❯ ' --marker='*' --pointer='->' --separator='─' --scrollbar='│' --layout='reverse-list' --info='right' --height 30"

   fzf/executable "fzf"
   fzf/git-grep-args "-i --line-number %s"
   ;; command used for `fzf-grep-*` functions
   ;; example usage for ripgrep:
   ;; fzf/grep-command "rg --no-heading -nH"
   fzf/grep-command "grep -nrH"
   ;; If nil, the fzf buffer will appear at the top of the window
   fzf/position-bottom t
   fzf/window-height 30))

;; Skip the prompt for delete the buffer.
(defun my/always-kill-buffer-with-process ()
  "Override to always kill buffer with a running process without prompt." t)

(advice-add 'process-kill-buffer-query-function :override #'my/always-kill-buffer-with-process)

(defun fzf-find-file (&optional directory)
  "Find a file using fzf. Optionally start from DIRECTORY."
  (interactive "DDirectory: ")  ;; Prompt for directory if not passed
  (let ((d (or directory default-directory)))
    ;; Change the current directory to the specified one
    (let ((default-directory (expand-file-name d)))
      ;; Start fzf in the specified directory
      (fzf default-directory))
    ;; Bind ESC to quit the fzf buffer and close the window
    (with-current-buffer "*fzf*"
      (local-set-key (kbd "<escape>") 'fzf-quit))))

(defun fzf-quit ()
  "Quit the fzf process, clean up the buffer, and close the window."
  (interactive)
  (let ((buffer (get-buffer "*fzf*"))
        (window (get-buffer-window "*fzf*")))
    (when buffer
      ;; Kill the buffer
      (kill-buffer buffer))
    (when window
      ;; Delete the window where fzf was opened
      (delete-window window))))

;;; LATEX
;; LaTeX Classes
(with-eval-after-load 'ox-latex
  ;; Add custom class for: Manuals
  (add-to-list 'org-latex-classes
               '("manuals"
                 "\\documentclass[a4paper,12pt]{article}  [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]

                \\usepackage{fontspec}
                \\usepackage[scaled=1]{gentium} \\renewcommand\\familydefault{\\rmdefault}
                \\usepackage[scaled=.90]{cascadia-code} \\renewcommand*\\familydefault{\\ttdefault}
                \\usepackage[scaled=.85,tabular,lining]{montserrat} \\renewcommand*\\familydefault{\\sfdefault}

                \\usepackage[a4paper, left=1in, right=1in, top=1in, bottom=1in]{geometry}
                \\setlength{\\textheight}{9.5in}
                \\setlength{\\textwidth}{6.5in}

                \\usepackage{hyperref}
                \\hypersetup{
                    colorlinks,
                    citecolor=gray,
                    filecolor=orange,
                    linkcolor=black,
                    urlcolor=NavyBlue
                }
                \\usepackage{bookmark}

                \\usepackage{minted}
                \\usepackage[dvipsnames]{xcolor}
                \\usepackage{listings}

                \\usepackage{fancyhdr}
                \\usepackage{lastpage}
                \\pagestyle{fancy}
                \\fancyhf{}
                \\fancyhead[R]{\\bf{\\leftmark}}
                \\fancyfoot[C]{\\thepage{} of \\pageref{LastPage}}
                \\fancyfoot[R]{ Javier Pacheco }

                \\AddToHook{cmd/section/before}{\\clearpage}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("exam"
                 "\\documentclass[11pt,addpoints]{exam} [NO-DEFAULT-PACKAGES]
                \\usepackage{graphicx}
                \\usepackage{pgf,tikz,pgfplots}
                \\pgfplotsset{compat=1.15}
                \\usepgfplotslibrary{fillbetween}
                \\pointpoints{punto}{puntos}
                \\pagestyle{headandfoot}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-listings 't)
(setq TeX-engine 'xetex)

(use-package auctex
  :ensure t)

(setq org-export-allow-bind-keywords t)

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes
(setq TeX-command-extra-options "-shell-escape")

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-listings 'minted)       ;; Use minted for code blocks
(setq org-latex-minted-options          ;; Here you add the options
      '(
        ("linenos" "true")				;; Enable line numbers.
        ("numbersep" "2pt")				;; separation of numbers.
        ("breaklines" "true")				;; enable breaklines.
        ;; ("frame" "leftline")				;; Add a leftline to the frame.
        ;; ("framerule" "2pt")				;; Weight of the leftline.
        ;; ("labelposition" "bottomline")	;; Position of label.
        ("bgcolor" "GreenYellow!20")

        ))		;; color and level of transparency.


;;; SPELL

(if lpr-windows-system
    (setenv "LANG" "en_US, es_MX"))
(if lpr-windows-system
    (setenv "DICPATH"
            (concat (getenv "HOME") ".emacs.d/lang")))
(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "~/.emacs.d/lang/en_US.aff")
        ("es_MX" "~/.emacs.d/lang/es_MX.aff")))

(if lpr-windows-system
    ;;; Windows
    (setq ispell-local-dictionary-alist
          ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
          ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
            ("es_MX" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "es_MX") nil utf-8)))
    ;;; Linux
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
          ("es_MX" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")


;; ;; Change betwen English and Spanish,
;; ;; English is he default.
(defvar ispell-current-dictionary "en_US")

(defun toggle-ispell-dictionary ()
  (interactive)
  (if (string= ispell-current-dictionary "en_US")
      (progn
        (setq ispell-current-dictionary "es")
        (message "Switched to Spanish dictionary"))
    (progn
      (setq ispell-current-dictionary "en_US")
      (message "Switched to English dictionary")))
  (ispell-change-dictionary ispell-current-dictionary))

;; (global-set-key (kbd "<f8>") 'toggle-ispell-dictionary)

(when (eq system-type 'gnu/linux)
  (use-package jinx
    :ensure t
    :hook (text-mode . jinx-mode)
    :bind (("M-;" . jinx-correct)
           ("<f8>" . jinx-languages))))
(add-hook 'text-mode-hook #'jinx-mode)


;;; PDF
(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-page-command)
              ("k" . pdf-view-previous-page-command))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook (blink-cursor-mode -1))

(defun my-evil-pdf-view-keybindings ()
  (evil-define-key 'normal doc-view-mode-map
    "j" 'pdf-view-next-page-command
    "k" 'pdf-view-previous-page-command))

(add-hook 'pdf-view-mode-hook 'my-evil-pdf-view-keybindings)

(use-package doc-view
  :custom
  (doc-view-resolution 200)
  (doc-view-mupdf-use-svg t)
  (large-file-warning-threshold (* 50 (expt 2 20)))
  :bind
  (:map doc-view-mode-map
        ("j" . doc-view-next-page)
        ("k" . doc-view-previous-page)))

(defun my-evil-doc-view-keybindings ()
  (evil-define-key 'normal doc-view-mode-map
    "j" 'doc-view-next-page
    "k" 'doc-view-previous-page))

(add-hook 'doc-view-mode-hook 'my-evil-doc-view-keybindings)

(provide 'init)
;;; init.el ends here
