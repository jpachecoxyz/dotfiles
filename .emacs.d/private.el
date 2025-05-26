;;; private.el --- my Custom Emacs Configuration --- Private  -*- lexical-binding: t; -*-


;;; Commentary:
;;  My custom packages configurations

;;; Code:

;; Initialize package sources
;; Install use-package support
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

(use-package no-littering
  :ensure t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; UTILITIES
;; (message (expand-file-name (concat user-emacs-directory "elisp/utilities.el")))
(use-package utilities
  :load-path "~/.emacs.d/elisp"
  :defer 5
  :init (message "utilities loaded correctly"))

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
  :custom
  (setq completion-styles '(orderless)))

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
              (find-file "~/.emacs.d/private.el"))
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

;; (use-package vertico-posframe
;;   :ensure t)

;; (add-to-list 'vertico-multiform-categories
;;              '(jinx grid (vertico-grid-annotate . 20)))
;; (vertico-multiform-mode 1)

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

;; (use-package fill-column-indicator
;;   :ensure nil
;;   :config
;;     (set-face-background 'fill-column-indicator "white"))

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

(provide 'private)
;;; private.el ends here
