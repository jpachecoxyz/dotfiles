;; ;; Initialize package sources
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'buffer-move)   	;; Buffer-move for better window management
;; (require 'utilities)		;; Custom scripts
;; (require 'term-toggle)	;; toggle-term
;; (require 'nano-splash)	;; Splash screen

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

(use-package fontaine
  :ensure t
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))

  (fontaine-presets
   '(
	 (laptop
	  :default-height 122)
	 (work
	  :default-height 122)
	 (pre-work
	  :default-height 200)
	 (pre-lap
	  :default-height 230)
	 (t
	  :default-family "JetBrains Mono"
	  :default-weight regular
	  :default-height 100
	  :fixed-pitch-family nil ; falls back to :default-family
	  :fixed-pitch-weight nil ; falls back to :default-weight
	  :fixed-pitch-height 1.0
	  :fixed-pitch-serif-family nil ; falls back to :default-family
	  :fixed-pitch-serif-weight nil ; falls back to :default-weight
	  :fixed-pitch-serif-height 1.0
	  :variable-pitch-family nil
	  :variable-pitch-weight nil
	  :variable-pitch-height 1.0
	  :bold-family "IBM Plex" ; use whatever the underlying face has
	  :bold-weight bold
	  :italic-family nil;
	  :italic-weight italic;
	  :italic-family "JetBrains Mono"
	  :italic-slant italic
	  :line-spacing nil)))

  :config
  ;; Recover last preset or fall back to desired style from
  (if lpr-windows-system
	  (fontaine-set-preset 'work)
	(or (fontaine-set-preset 'laptop)))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'after-load-theme-hook #'fontaine-restore-latest-preset))

(define-key global-map (kbd "C-c f") #'fontaine-set-preset)
(define-key global-map (kbd "C-c F") #'fontaine-set-face-font)

;; Load modeline
;; (require 'custom-modeline)
;; (custom-modeline-mode)
;; (setq custom-modeline-segments
;;       `((custom-modeline-segment-modified	;; icon if file is modified 
;; 		 custom-modeline-segment-buffer-name)	;; buffer name
;; 		(custom-modeline-segment-major-mode 	;; Major mode
;; 		 custom-modeline-segment-vc 			;; vc status
;; 		 custom-modeline-segment-misc-info 	;; Clock and frame name
;; 		 custom-modeline-segment-process)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment lsp
    "Displays LSP server status."
    (when (and (bound-and-true-p lsp-mode) (lsp-workspaces))
      (concat
       (propertize (doom-modeline-spc) 'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))
       (propertize (nerd-icons-mdicon "nf-md-repeat")
                   'face `(:family ,(nerd-icons-mdicon-family) :inherit))
                   ;; 'display '(raise -0.1))
       (propertize (doom-modeline-spc) 'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))))

(use-package hide-mode-line
  :ensure t
  :defer t)

(setq doom-modeline-icon nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-time-analogue-clock nil)
(setq doom-modeline-position-line-format nil)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-percent-position '(-3 "%p"))
(setq display-time-default-load-average nil)
(setq display-time-load-average nil)
(display-time-mode -1) ;; displays current time
;; Default custom modeline
(setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
(setq doom-modeline-modal-icon nil)
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
(global-hl-line-mode 1)

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))

(use-package diminish
  :ensure t)

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annonators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

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

(use-package pretty-mode
  :defer t 
  :ensure t
  :hook
  (prog-mode . pretty-mode))

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
  evil-want-keybinding nil
  evil-vsplit-window-right t
  evil-split-window-below t
  evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
      (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
      (evil-collection-init))

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

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.5 . 0.5))
  (corfu-preview-current 'valid) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-right-margin-width 1.0) ; Give some margin to align with nerd-icons left margin.
  (corfu-left-margin-width 1.0)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
		("TAB" . corfu-next)
		([tab] . corfu-next)
		("S-TAB" . corfu-previous)
		([backtab] . corfu-previous))

  :init
  ;; (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
			(lambda () (setq-local corfu-quit-at-boundary t
							  corfu-quit-no-match t
							  corfu-auto nil) (corfu-mode)) nil t)

  :hook (
         (prog-mode . corfu-mode)))


;; (use-package corfu-doc
;;   :after corfu
;;   :config
;;   (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
;;   (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
;;   (setq corfu-doc-display-within-parent-frame nil)
;;   (add-hook 'corfu-mode-hook #'corfu-doc-mode))
;; (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

;; Icons
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq kind-icon-mapping
	  '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
		(boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
		(color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
		(command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
		(constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
		(class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
		(constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
		(enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
		(enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
		(enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
		(event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
		(field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
		(file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
		(folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
		(function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
		(interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
		(keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
		(macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
		(magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
		(method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
		(module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
		(numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
		(operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
		(param          "pa"  :icon "gear"               :face default                          :collection "vscode")
		(property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
		(reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
		(snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
		(string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
		(struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
		(text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
		(typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
		(type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
		(unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
		(value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
		(variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
		(t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))

;; yasnippets integration with corfu.
(use-package yasnippet-capf
  :after cape)

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Function to prioritize yasnippet-capf in completion-at-point-functions
(defun my-prioritize-yasnippet-capf ()
  "Ensure `yasnippet-capf` is the first in `completion-at-point-functions`."
  (let ((yas-capf (car (remove 'yasnippet-capf completion-at-point-functions))))
    (setq completion-at-point-functions
          (cons 'yasnippet-capf (remove 'yasnippet-capf completion-at-point-functions)))))

;; Hook the function to major modes where you want this behavior
(add-hook 'prog-mode-hook #'my-prioritize-yasnippet-capf)
(add-hook 'text-mode-hook #'my-prioritize-yasnippet-capf)

;; Ensure Corfu appears in the right position
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)))

(use-package posframe
  :ensure t)

(setq default-directory "~/")
;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link  t)

;; General Settings
(fset 'yes-or-no-p 'y-or-n-p) ;; never type: yes or no

;;; set transparency to 90
(if (eq system-type 'windows-nt)
	(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (if (eq system-type 'gnu/linux)
	  (set-frame-parameter nil 'alpha-background 90)
	(add-to-list 'default-frame-alist '(alpha-background . 90))))

(defun toggle-alpha-transparency ()
  "Toggle transparency based on the operating system."
  (interactive)
  (if (eq system-type 'windows-nt)
      (if (equal (frame-parameter nil 'alpha) '(90 . 90))
          (progn
            (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
            (add-to-list 'default-frame-alist '(alpha . (100 . 100))))
        (progn
          (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
          (add-to-list 'default-frame-alist '(alpha . (90 . 90)))))
    (if (eq system-type 'gnu/linux)
        (if (equal (frame-parameter nil 'alpha-background) 90)
            (progn
              (set-frame-parameter nil 'alpha-background 100)
              (add-to-list 'default-frame-alist '(alpha-background . 100)))
          (progn
            (set-frame-parameter nil 'alpha-background 90)
            (add-to-list 'default-frame-alist '(alpha-background . 90)))))))

;; (toggle-alpha-transparency)
;; (global-set-key (kbd "<f2>") 'toggle-alpha-transparency)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-message t)
(setq org-hide-emphasis-markers t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq org-src-window-setup 'current-window)
(electric-indent-mode 0)
(electric-layout-mode 0)
(setq org-edit-src-content-indentation 0)
(setq enable-recursive-minibuffers t)
(setq display-time-day-and-date t)
(display-time-mode 1)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<f12>") 'jp-themes-load-random)
(setq org-display-inline-images t)
(setq org-image-actual-width (list 550))
(setq visible-bell nil)
;;;; Set the fill column to 80
(setq-default fill-column 80)
;; Enable display-fill-column-indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) 

;;Doom insert item below
(require 'bind-key)
(bind-key* "<C-return>" '+org/insert-item-below)
(bind-key* "<C-S-return>" '+org/insert-item-above)

(setq confirm-kill-emacs nil)
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))
(global-set-key [escape] 'keyboard-escape-quit)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(global-display-line-numbers-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                help-mode-hook
                org-mode-hook
                telega-chat-mode-hook
                telega-root-mode-hook
                eww-mode-hook
                pdf-view-mode-hook
                newsticker-treeview-mode-hook
                newsticker-treeview-mode-item-hook
                newsticker-treeview-mode-item-hook
                devdocs-mode-hook
                gnus-group-mode-hook
                gnus-summary-mode-hook
                org-agenda-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)  ;; Enable truncated lines
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

;; Windows rules:
(setq org-agenda-window-setup 'switch-to-buffer-other-window)
(add-to-list 'display-buffer-alist '("\\*Agenda Commands\\*"
                                     (display-buffer-in-side-window)
                                     (side . bottom)
                                     (slot . 6)
				     	(window-width 1.0)
                                     (dedicated . t)))
(add-to-list 'display-buffer-alist '("\\*Org Select\\*"
                                     (display-buffer-in-side-window)
                                     (side . bottom)
                                     (slot . 6)
				     	(window-width 1.0)
                                     (dedicated . t)))

(defalias 'lp 'list-packages)
(defalias 'pi 'package-install)

;; LaTeX Classes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
				 "\\documentclass{article} [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
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

(setq org-latex-listings 'minted) 		;; Use minted for code blocks
(setq org-latex-minted-options 			;; Here you add the options 
      '(("linenos" "true")				;; Enable line numbers.
		("numbersep" "2pt")				;; separation of numbers.
		("breaklines" "true")				;; enable breaklines.
		("frame" "leftline")				;; Add a leftline to the frame.
		("framerule" "2pt")				;; Weight of the leftline.
		("labelposition" "bottomline")	;; Position of label.
		("bgcolor" "jpyellow!70")))		;; color and level of transparency.

(use-package olivetti
  :ensure t
  :defer t
  :custom
  (olivetti-body-width 0.7))
  ;; :hook (org-mode . olivetti-mode))

(global-set-key (kbd "<f1>") 'olivetti-mode)

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
  :defer t)

(use-package vundo
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

(use-package git-gutter
  :init (global-git-gutter-mode 1)
  :defer t
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package transient
  :defer t)

(setq vc-git-root-log-format
    `("%d %h %ai %an: %s"
        ;; The first shy group matches the characters drawn by --graph.
        ;; We use numbered groups because `log-view-message-re' wants the
        ;; revision number to be group 1.
        ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                "\\(?3:.*?\\):")
        ((1 'log-view-message)
        (2 'change-log-list nil lax)
        (3 'change-log-name)
        (4 'change-log-date))))

(use-package hydra
  :ensure t)

(use-package major-mode-hydra
  :after hydra)

(defun with-alltheicon (icon str &optional height v-adjust face)
  "Display an icon from all-the-icon."
  (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

(defun with-faicon (icon str &optional height v-adjust face)
  "Display an icon from Font Awesome icon."
  (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

(defun with-fileicon (icon str &optional height v-adjust face)
  "Display an icon from the Atom File Icons package."
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

(defun with-octicon (icon str &optional height v-adjust face)
  "Display an icon from the GitHub Octicons."
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

(pretty-hydra-define hydra-launcher
  (:hint nil :color teal :quit-key ("q" "<escape>") :title (with-octicon "rocket" "Hydra Launcher " 1 -0.05))
  ("Launch"
   (("h" man "man")
	("j" (browse-url "https://jpacheco.xyz") "Jpacheco")
	("w" (browse-url "http://www.emacswiki.org/") "emacswiki")
	("g" (browse-url "http://www.google.com") "Google")
	("l" (browse-url "https://autoliv-mx.leading2lean.com/") "L2L")
	("p" (browse-url "http://erp/cgi-bit/rpgle/cgipartvw3.pgm") "Part-view")
	("s" shell "shell"))))
(global-set-key (kbd "C-c g") 'hydra-launcher/body)

(pretty-hydra-define hydra-yasnippet 
  (:nit nil :color teal :quit-key ("q" "<escape>") :title (with-octicon "code" "Yasnippet" 1 -0.05))
  ("Yasnippet Menu"
   (("i" yas-insert-snippet)
	("e" yas-visit-snippet-file)
	("m" yas-minor-mode)
	("n" yas-new-snippet))))
(global-set-key (kbd "C-c y") 'hydra-yasnippet/body)

(pretty-hydra-define hydra-agenda-files
  (:hint nil :color teal :quit-key ("q" "<escape>") :title (with-faicon "book" "Open org Agenda files" 1 -0.05))
  ("Personal Agenda"
   (("b" (find-file "~/public/org/agenda/bdays.org") "Birthdays")
	("p" (find-file "~/public/org/agenda/personal.org") "Personal")
	("t" (find-file "~/public/org/agenda/training.org") "Training")
	("n" (find-file "~/public/org/agenda/notes.org") "Notes"))
   "Work Agenda"
   (("m" (find-file "~/public/org/agenda/pm.org") "P.Maintenance")
	("r" (find-file "~/public/org/agenda/refill.org") "Refill")
	("w" (find-file "~/public/org/agenda/work.org") "Work"))))

(pretty-hydra-define hydra-org-capture
  (:hint nil :color teal :quit-key ("q" "<escape>") :title (with-faicon "sticky-note" "Org roam notes" 1 -0.05))
  ("Org-roam commands"
   (("c" org-roam-capture "Capture to Org Roam")
	("f" org-roam-node-find "Find Org Roam node")
	("g" org-roam-node-insert "Insert Org Roam node")
	("i" org-roam-node-insert-immediate "Insert Org Roam node (immediate)")
	("j" org-roam-dailies-capture-today "Capture today's dailies")
	("l" org-roam-buffer "Show all Org Roam files")
	("s" jp/search-roam "Search Nodes using rgrep")
	("t" org-roam-tag-add "Add tag to Org Roam node"))
   "Org-roam dailies"
   (("1" org-roam-dailies-capture-today "Daily today")
	("2" org-roam-dailies-capture-tomorrow "Daily tomorrow")
	("3" org-roam-dailies-capture-yesterday "Daily yesterday")
	("d" org-roam-dailies-capture-date "Insert Org Roam on a date"))
   "Org-roam-ui"
   (("u" org-roam-ui-open "Open Org-roam-ui"))))
(global-set-key (kbd "C-c o") 'hydra-org-capture/body)

(pretty-hydra-define hydra-buffers
  (:color teal
		  :quit-key ("q" "<escape>")
		  :hint nil
		  :title (with-octicon "book" "Hydra itter buffer" 1 -0.05))
  ("Move arround buffers"
   (("j" evil-next-buffer :color red)
	("k" evil-prev-buffer :color red))
   "Resize buffers"
   (("h" shrink-window-horizontally "Shrink horizontally" :color red)
	("l" enlarge-window-horizontally "Enlarge horizontally" :color red))
   "Kill Buffers"
   (("K" kill-current-buffer :color red))
   "Exit"
   (("RET" nil "cancel"))))
(global-set-key (kbd "C-c b") 'hydra-buffers/body)

(defvar jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))
(pretty-hydra-define jp-toggles
  (:color amaranth :quit-key ("q" "<escape>") :title jp-toggles--title)
  ("Basic"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("p" toggle-alpha-transparency "Toggle thransparency" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("S" pretty-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("T" hl-todo-mode "todo" :toggle t))
   "Coding"
   (("f" flycheck-mode "flycheck" :toggle t)
	("t" term-toggle-term "Terminal" :toggle t))
   "Emacs/org"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
	("e" jp/org-toggle-emphasis-markers "Toggle emphasis" :toggle t)
	("o" olivetti-mode "Toggle Olivetti mode" :toggle t)
	("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
   ))
(global-set-key (kbd "<f3>") 'jp-toggles/body)

(pretty-hydra-define hydra-of-hydras 
  (:hint nil :color teal :quit-key ("q" "<escape>") :title (with-faicon "sliders" "Hydra menu" 1 -0.05))
  ("Hydra Menu selector"
   (("a" hydra-agenda-files/body "Agenda files menu")
	("b" hydra-buffers/body "Buffer menu")
	("l" hydra-launcher/body "Launcher menu")
	("t" jp-toggles/body "Toggle menu")
	("y" hydra-yasnippet/body "Yasnippets menu"))))
(global-set-key (kbd "C-c m") 'hydra-of-hydras/body)

(setq hydra-posframe-border-width 3)
(setq hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center)

(require 'hydra-posframe)
(require 'hydra-themes)
(hydra-posframe-mode)

(use-package hl-todo
  :defer t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
		hl-todo--regex "\\(\\<\\(TODO\\|DOING\\|FIXME\\|HACK\\|REVIEW\\|NOTE\\|DEPRECATED\\)\\>[:]*\\)"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
		  ("DOING"		warning bold italic)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face italic)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(setq hl-todo-keyword-faces
	  '(("TODO"   .		"#de935f")
		("DOING"  .		"#8abeb7")
		("FIXME"  . 	"#dc6666")
		("HACK"  . 		"#fe935f")
		("REVIEW"  . 	"#c3c322")
		("NOTE" . 		"#f1f1f1")
		("DEPRECATED" . "#fc5555")))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :defer t
  :hook ((org-mode prog-mode) . rainbow-mode))

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

(use-package dired
  :ensure nil
  :after evil-collection
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-open-file))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "nsxiv")
                                ("jpg" . "nsxiv")
                                ("png" . "nsxiv")
                                ;; ("pdf" . "zathura")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

;; (use-package dired-preview
;;   :ensure t
;;   :after dired
;;   :hook (dired-mode . dired-preview-mode))

;; (defun my-dired-preview-to-the-right ()
;;   "My preferred `dired-preview-display-action-alist-function'."
;;   '((display-buffer-in-side-window)
;;     (side . right)
;;     (window-width . 0.4)))

;; (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
;; (setq dired-preview-delay 0.1)

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Set speller and dicts
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

;; flyspell spellcheck on the fly...
(use-package flyspell
  :defer t
  ;;:delight
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-mode 1))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
      ("M-\\" . flyspell-correct-word-before-point))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package ispell
  :custom
  (ispell-silently-savep t))

;; Activate spellcheck in text mode, org, txt files etc...
(add-hook 'text-mode-hook
  '(lambda () (flyspell-mode 1)))

;; Change betwen English and Spanish, 
;; English is he default.
(defvar ispell-current-dictionary "en_US")

(defun toggle-ispell-dictionary ()
  (interactive)
  (if (string= ispell-current-dictionary "en_US")
      (progn
        (setq ispell-current-dictionary "es_MX")
        (message "Switched to Spanish dictionary"))
    (progn
      (setq ispell-current-dictionary "en_US")
      (message "Switched to English dictionary")))
  (ispell-change-dictionary ispell-current-dictionary))

(global-set-key (kbd "<f8>") 'toggle-ispell-dictionary)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("⁖" "⁖" "⁖" "⁖" "⁖" "○" "●")))

(use-package org-download
  :ensure t)

(require 'org-tempo)
(require 'org-id)
(setq org-id-link-to-org-use-id 'use-existing)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-x x i") 'jp/org-id-headline)
(global-set-key (kbd "C-x x I") 'jp/org-id-headlines)

(require 'cycle-region)
(cycle-region-mode)
(add-hook 'cycle-region-post-preview-hook 'evil-normal-state)

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package org-mime
  :ensure t)

(setq org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil))

(add-hook 'message-send-hook 'org-mime-htmlize)
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
            "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                          "#E6E1DC" "#232323"))))

;; Automatically tangle this config.org  file when we save it
(defun jp/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jp/org-babel-tangle-config)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-startup-folded 'overview)
(setq org-support-shift-select t)
(setq org-log-done 'time)
(setq org-hide-emphasis-markers t)
(setq org-log-into-drawer t)
(setq org-ellipsis " †")
(setq org-directory "~/public/org/")
(setq org-tag-alist
 '(;;Places
 ("@home" . ?h)
 ("@work" . ?w)
 ("@kindergardeen" . ?k)

 ;; Whom
 ("lia" . ?l)
 ("jr" . ?j)
 ("xiomara" . ?x)
 
 ;; Devices
 ("@laptop" . ?L)
 ("@phone" . ?p)

 ;; Activities
 ("programming" . ?P)
 ("planning" . ?n)
 ("writting" . ?W)
 ("calls" . ?c)
 ("email" . ?e)
 ("errands" . ?r)))
(setq org-agenda-files
      '("~/public/org/agenda/personal.org"
        "~/public/org/agenda/training.org"
        "~/public/org/agenda/bdays.org"
        "~/public/org/agenda/important_dates.org"
        "~/public/org/agenda/contacts.org"
        "~/public/org/agenda/work.org"
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

(setq org-refile-targets
	  '((nil :maxlevel . 1)
		("~/webdev/jpachecoxyz.github.io/hugo/org/jpacheco.xyz.org" :maxlevel . 1)	
		("personal.org" :maxlevel . 1)
		("work.org" :maxlevel . 1)))

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

(use-package org-contacts
  :ensure t)

(defvar my/org-contacts-template "* %(org-contacts-template-name)
   :PROPERTIES:
   :EMAIL: %(org-contacts-template-email)
   :PHONE: %^{Telefono}
   :IGNORE:
   :NOTE: %^{NOTA}
   :BIRTHDAY: %^{Cumpleaños}
   :END:" "Plantilla para org-contacts.")

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
                (org-agenda-overriding-header "\nToday's agenda\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 5)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                ;; (org-agenda-entry-types '(:deadline))
                (org-agenda-overriding-header "\nNext five days\n")))
    ;; (agenda "" ((org-agenda-time-grid nil)
    ;;             (org-agenda-start-on-weekday nil)
    ;;             ;; We don't want to replicate the previous section's
    ;;             ;; three days, so we start counting from the day after.
    ;;             (org-agenda-start-day "+6d")
    ;;             (org-agenda-span 14)
    ;;             (org-agenda-show-all-dates nil)
    ;;             (org-deadline-warning-days 0)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-entry-types '(:scheduled))
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
    ;;             (org-agenda-overriding-header "\nUpcoming tasks (+14d)\n")))
	
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
		;; 					  "~/public/org/agenda/training.org"))
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
				  ((org-agenda-overriding-header "Birthday Calendar dates\n")
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
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-track-ordered-property-with-tag t)
(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)

(use-package calfw
  :ensure t
  :config
  (setq cfw:org-overwrite-default-keybinding t)
  (setq cfw:fchar-junction ?╋
		cfw:fchar-vertical-line ?┃
		cfw:fchar-horizontal-line ?━
		cfw:fchar-left-junction ?┣
		cfw:fchar-right-junction ?┫
		cfw:fchar-top-junction ?┯
		cfw:fchar-top-left-corner ?┏
		cfw:fchar-top-right-corner ?┓))

(use-package calfw-org
  :ensure t
  ;; :bind ([f5] . cfw:open-org-calendar)
  :config
  (calendar-holidays '((holiday-fixed 1 1 "Año Nuevo")
					   (holiday-fixed 1 6 "Reyes Magos")
					   (holiday-fixed 5 1 "Dia del Trabajo")
					   (holiday-fixed 11 01 "Todos los Santos")
					   (holiday-fixed 11 02 "Da de Muertos")
					   (holiday-fixed 12 25 "Navidad"))))

;; Capture
(setq org-default-notes-file '("~/public/org/agenda/refill.org"))
(global-set-key (kbd "C-c c") 'org-capture)      ;; use C-c c to start capture mode

;; capture templates for: TODO tasks, Notes, appointments, meetings
(setq org-templates-location-var (concat org-directory "agenda/refill.org"))

(defun jpacheco/org-capture-new-post ()
  (let ((filename (read-string "Enter the name of the file (without extension): ")))
    (expand-file-name (concat filename ".org") "~/repos/jpacheco.xyz/content/posts/")))

(setq org-capture-templates
      `(
		("p" "Posts")
		("pa" "Add a Post"
         entry
         ;; File path and headline where the captured post should be stored.
         (file+headline "~/webdev/jpachecoxyz.github.io/hugo/org/jpacheco.xyz.org" "Posts:")
         (function org-hugo-new-subtree-post-capture-template) :empty-lines-before 2)

		("pi" "Idea for a Post"
		 entry (file+headline "~/webdev/jpachecoxyz.github.io/hugo/org/jpacheco.xyz.org" "Ideas:")
		 "* TODO %^{Name of the post: }%?" :empty-lines-before 2)

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

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/public/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n n" . org-roam-node-find)
         ("C-c n b" . org-mark-ring-goto)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; (setq org-roam-node-display-template
;;       (concat "${title:*} "
;;               (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
	  '(
		;; Notes
		("n" "Notes")
		("np" "Personal")
		;; Personal
		("npp" "Personal note" plain (file "~/public/roam/templates/notes.org")
		 :if-new (file+head "1.1_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 :posframe t 
		 :unnarrowed t)
		;; Tecnologia
		("npt" "Technology" plain (file "~/public/roam/templates/notes.org")
		 :if-new (file+head "1.2_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 :unnarrowed t)

		;; Trabajo
		("nw" "Work")
		;; Autoliv
		("nwc" "Cell" plain (file "~/public/roam/templates/work.org")
		 :if-new (file+head "2.1_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 :unnarrowed t)
		;; equipo
		("nwe" "Equipment" plain (file "~/public/roam/templates/equipo.org")
		 :if-new (file+head "2.1.1_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 :unnarrowed t)
		;; documentacion
		("nwd" "Documentation" plain (file "~/public/roam/templates/documentation.org")
		 :if-new (file+head "2.1.2_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 :unnarrowed t)
		;; SyMA
		("nws" "Syma" plain (file "~/public/roam/templates/syma.org")
		 :if-new (file+head "2.2_${slug}.org" "#+title: ${title}\n")
         :no-save t
         :immediate-finish nil
         :kill-buffer t
		 r
		 :unnarrowed t)

		))
;; UI
(use-package org-roam-ui
  ;; :hook (after-init . org-roam-ui-mode)
  :defer t
  :bind (("C-c n u" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Bind this to C-c n I
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(use-package org-fancy-priorities
  :ensure t
  :hook ((org-agenda-mode . org-fancy-priorities-mode)
   (org-mode . org-fancy-priorities-mode))
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(setq org-fancy-priorities-list '((?A . "❗")
                                  (?B . "⬆")
                                  (?C . "⬇")
                                  (?D . "☕")
                                  (?1 . "⚡")
                                  (?2 . "⮬")
                                  (?3 . "⮮")
                                  (?4 . "☕")
                                  (?I . "Important")))

(use-package org-sidebar
  :ensure t)

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package hide-lines
  :ensure t
  :defer t)

(defun terror/slide-setup ()
  (global-hl-line-mode -1)
  (org-bullets-mode 1)
  (setq text-scale-mode-amount 2)
  (text-scale-mode 1)
  (set-frame-parameter (selected-frame)
                       'internal-border-width 50)
  (org-display-inline-images)
  (toggle-frame-fullscreen)
  (hide-mode-line-mode 1)
  (hide-lines-matching "#\\+begin_src")
  (hide-lines-matching "#\\+end_src"))

(defun terror/slide-end ()
  (global-hl-line-mode 1)
  (setq text-scale-mode-amount 0)
  (text-scale-mode -1)
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
		org-tree-slide-activate-message "Let's begin..."
		org-tree-slide-deactivate-message "The end."))

(global-set-key (kbd "<f7>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f7>") 'org-tree-slide-skip-done-toggle)
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f1>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f2>") 'org-tree-slide-move-next-tree))

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'isearch)
  (pulsar-global-mode 1)
  :bind ("<f2>" . pulsar-pulse-line))

(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-page)
              ("k" . pdf-view-previous-page))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook (blink-cursor-mode -1))

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (doc-view-mupdf-use-svg t)
  (large-file-warning-threshold (* 50 (expt 2 20))))

(use-package tree-sitter
  :ensure t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package treesit-ispell
  :ensure t
  :defer t
  :bind (("C-x C-s" . treesit-ispell-run-at-point)))

(with-eval-after-load 'treesit
  (setq treesit-font-lock-level 4))

(defun jp/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . jp/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-treemacs-symbols-position-params '((side . right) (slot . 2) (window-width . 35)))
  (setq lsp-idle-delay 0.0))

(defun toggle-lsp-treemacs-symbols ()
  "Toggle the visibility of the lsp-treemacs-symbols buffer."
  (interactive)
  (let ((buffer (get-buffer "*LSP Symbols List*")))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (lsp-treemacs-symbols))))

(global-set-key (kbd "<f5>") 'toggle-lsp-treemacs-symbols)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package ligature
  :defer 1
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))
  (global-ligature-mode t))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")

(use-package yasnippet
  :defer 2
  :init (yas-reload-all)
  :custom (yas-keymap-disable-hook (lambda () (frame-visible-p corfu--frame)))
  :hook ((prog-mode . yas-minor-mode)
	 	 (org-mode . yas-minor-mode)))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :ensure t)

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
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
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

;; Enable vertico
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

(use-package shackle
  :custom
  ((shackle-rules
    (let ((repls "\\*\\(cider-repl\\|sly-mrepl\\|ielm\\)")
          (vcs   "\\*\\(Flymake\\|Package-Lint\\|vc-\\(git\\|got\\) :\\).*")
	      (docs "\\*devdocs\\*")
	      (roam "\\*Capture\\*")
	      (warnings "\\*Warnings\\*")
	      (magit "magit-diff:*")
          (scratch    "\\*scratch\\*"))
      `((compilation-mode :noselect t
                          :align above
                          :size 0.2)
        ("*Async Shell Command*" :ignore t)
        (,repls :regexp t
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
        (,warnings :regexp t
                   :ignore t)
        ;; (,magit :regexp t
        ;;         :select t
		;; 		:align right
		;; 		:size 0.5)
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
		(,roam :regexp t 
			   :same t
			   :select t))))
   (shackle-default-rule nil ; '(:inhibit-window-quit t)
						 ))
  :config (shackle-mode))

(use-package form-feed
  :config (global-form-feed-mode))

(use-package page-break-lines
  :config
  (page-break-lines-mode))

(use-package htmlize
  :ensure t
  :defer t)

(use-package buffer-flip
  :defer t
  :ensure t
  :bind  (("C-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "C-<tab>" .   buffer-flip-forward) 
          ( "C-`" . buffer-flip-backward) 
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$"
          "^\\*Messages\\*$"
          "^\\*Warnings\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*rg\\*$"
          "^\\*Async-native-compile-log\\*$"
          "/\\'"
	  )))

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
   "SPC SPC" 'execute-extended-command
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
   ">" ">gv")

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

  (user/leader-keys
    "a" '(:ignore t :wk "Agenda buffers")
    "a" '(org-agenda :wk "Open the agenda"))

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
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f p" '((lambda () (interactive)
              (find-file "~/.emacs.d/config.org")) 
            :wk "Open noobemacs Configuraiton file.")
    "f r" '(recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
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
	"o -" '(dired-jump :wk "Dired jump to current")
	"o a" '(hydra-agenda-files/body :wk "Open org-agenda files")
	"o f" '(make-frame :wk "Open buffer in new frame")
	"o i" '(jp/org-id-store-link-for-headers :wk "Add ID's to org headers.")
	"o l" '(open-lisp-and-org-files :wk "Open lisp files")
    "o p" '((lambda () (interactive)
              (find-file "~/webdev/jpachecoxyz.github.io/hugo/org/jpacheco.xyz.org")) 
            :wk "Open web org file.")
	"o o" '(list-and-open-url-in-buffer :wk "Follow urls in buffer")
	"o t" '(term-toggle-term :wk "Toggle terminal")
	"o s" '(toggle-scratch-buffer :wk "Toggle scratch buffer")
	"o e" '(toggle-org-buffer :wk "Toggle org buffer")
	"o F" '(select-frame-by-name :wk "Select frame by name"))

;; projectile-command-map already has a ton of bindings 
;; set for us, so no need to specify each individually.
(user/leader-keys
  "p" '(:ignore t :wk "Projectile")
  "p a" '(projectile-add-known-project :wk "Add a project directory"))

(user/leader-keys
  "r" '(:ignore t :wk "Roam")
  "r f" '(org-roam-node-find :wk "Org rome node find")
  "r u" '(org-roam-ui-open :wk "Open org roam map")
  "r i" '(org-roam-node-insert :wk "Insert Node link")
  "r I" '(org-roam-node-insert-immediate :wk "Insert new Node link")
  )

(user/leader-keys
  "s" '(:ignore t :wk "Search")
  "s d" '(dictionary-search :wk "Search dictionary")
  "s m" '(man :wk "Man pages")
  "s t" '(tldr :wk "Lookup TLDR docs for a command")
  "s w" '(woman :wk "Similar to man but doesn't require man"))

(user/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t e" '(jp/org-toggle-emphasis-markers :wk "Toggle org-emphasis")
  "t f" '(flycheck-mode :wk "Toggle flycheck")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t o" '(org-mode :wk "Toggle org mode")
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

(use-package password-store
  :ensure t)

(use-package telega
  :ensure t
  :defer t
  :custom
  ;; enable markdown for code snippets
  (telega-server-libs-prefix "/home/javier/.nix-profile/")
  (telega-chat-input-markups '("markdown2" "org" nil))
  ;; use vertico for completion
  (telega-completing-read-function 'completing-read)
  :config
  ;; use shift enter to make multi line messages and enter to send it
  (define-key telega-chat-mode-map (kbd "S-<return>") #'newline)
  ;; avoid showing blank spaces highlighted
  (add-hook 'telega-chat-mode-hook #'(lambda ()
                                       (setq-local show-trailing-whitespace nil))))
(defun my-open-telega-and-chat-with ()
  "Open Telega and then run the command `telega-chat-with`."
  (interactive)
  ;; Ensure Telega is running
  (telega)
  ;; Wait a bit to ensure Telega is fully started
  (sleep-for 0.5)
  ;; Run telega-chat-with command
  (call-interactively 'telega-chat-with))

;; Optionally, bind the function to a key for quick access
(global-set-key (kbd "C-c t") 'my-open-telega-and-chat-with)