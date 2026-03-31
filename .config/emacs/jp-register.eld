;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((97) (113 . [escape escape])
 (48
  . #(";;; Mode line\n(jp-emacs-configure\n  (require 'jp-modeline)\n  (setq mode-line-compact nil) ; Emacs 28\n  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30\n  (setq jp-modeline-show-frame-name (alist-get 'undecorated initial-frame-alist))\n\n  (setq-default mode-line-format\n                '(\"%e\"\n                  jp-modeline-kbd-macro\n                  jp-modeline-narrow\n                  jp-modeline-buffer-status\n                  jp-modeline-window-dedicated-status\n                  jp-modeline-input-method\n                  \"  \"\n                  jp-modeline-buffer-identification\n                  \"  \"\n                  jp-modeline-major-mode\n                  jp-modeline-process\n                  \"  \"\n                  jp-modeline-frame-name\n                  \"  \"\n                  jp-modeline-vc-branch\n                  \"  \"\n                  jp-modeline-eglot\n                  \"  \"\n                  jp-modeline-flymake\n                  \"  \"\n                  mode-line-format-right-align ; Emacs 30\n                  jp-modeline-which-function-indicator\n                  jp-modeline-notmuch-indicator\n                  \"  \"\n                  jp-modeline-misc-info\n                  ))\n\n  (when jp-emacs-load-theme-family\n    (cond\n     ((memq jp-emacs-load-theme-family '(modus ef standard))\n      (defun jp/modeline-set-faces ()\n        (modus-themes-with-colors\n         (custom-set-faces\n          `(jp-modeline-indicator-red ((,c :inherit bold :foreground ,red)))\n          `(jp-modeline-indicator-green ((,c :inherit bold :foreground ,green)))\n          `(jp-modeline-indicator-yellow ((,c :inherit bold :foreground ,yellow)))\n          `(jp-modeline-indicator-blue ((,c :inherit bold :foreground ,blue)))\n          `(jp-modeline-indicator-magenta ((,c :inherit bold :foreground ,magenta)))\n          `(jp-modeline-indicator-cyan ((,c :inherit bold :foreground ,cyan)))\n          `(jp-modeline-indicator-red-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-red-intense :foreground ,fg-main)))\n          `(jp-modeline-indicator-green-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-green-intense :foreground ,fg-main)))\n          `(jp-modeline-indicator-yellow-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-yellow-intense :foreground ,fg-main)))\n          `(jp-modeline-indicator-blue-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-blue-intense :foreground ,fg-main)))\n          `(jp-modeline-indicator-magenta-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-magenta-intense :foreground ,fg-main)))\n          `(jp-modeline-indicator-cyan-bg ((,c :inherit (bold jp-modeline-indicator-button) :background ,bg-cyan-intense :foreground ,fg-main))))))\n      (add-hook 'modus-themes-after-load-theme-hook #'jp/modeline-set-faces))\n     ((eq jp-emacs-load-theme-family 'doric)\n      (defun jp/modeline-set-faces ()\n        (doric-themes-with-colors\n          (custom-set-faces\n           `(jp-modeline-indicator-red ((t :inherit bold :foreground ,fg-red)))\n           `(jp-modeline-indicator-green ((t :inherit bold :foreground ,fg-green)))\n           `(jp-modeline-indicator-yellow ((t :inherit bold :foreground ,fg-yellow)))\n           `(jp-modeline-indicator-blue ((t :inherit bold :foreground ,fg-blue)))\n           `(jp-modeline-indicator-magenta ((t :inherit bold :foreground ,fg-magenta)))\n           `(jp-modeline-indicator-cyan ((t :inherit bold :foreground ,fg-cyan)))\n           `(jp-modeline-indicator-red-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-red :foreground ,fg-main)))\n           `(jp-modeline-indicator-green-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-green :foreground ,fg-main)))\n           `(jp-modeline-indicator-yellow-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-yellow :foreground ,fg-main)))\n           `(jp-modeline-indicator-blue-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-blue :foreground ,fg-main)))\n           `(jp-modeline-indicator-magenta-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-magenta :foreground ,fg-main)))\n           `(jp-modeline-indicator-cyan-bg ((t :inherit (bold jp-modeline-indicator-button) :background ,bg-cyan :foreground ,fg-main))))))\n      (add-hook 'doric-themes-after-load-theme-hook #'jp/modeline-set-faces)))\n\n    (jp/modeline-set-faces))\n\n  (with-eval-after-load 'spacious-padding\n    (defun jp/modeline-spacious-indicators ()\n      \"Set box attribute to `'jp-modeline-indicator-button' if spacious-padding is enabled.\"\n      (if (bound-and-true-p spacious-padding-mode)\n          (set-face-attribute 'jp-modeline-indicator-button nil :box '(:style flat-button))\n        (set-face-attribute 'jp-modeline-indicator-button nil :box 'unspecified)))\n\n    ;; Run it at startup and then afterwards whenever\n    ;; `spacious-padding-mode' is toggled on/off.\n    (jp/modeline-spacious-indicators)\n\n    (add-hook 'spacious-padding-mode-hook #'jp/modeline-spacious-indicators)))\n\n;;; Show the name of the current definition or heading for context (`which-function-mode')\n(jp-emacs-configure\n  (setq which-func-modes '(prog-mode org-mode))\n  ;; NOTE 2025-10-26: I handle the indicator on my own via `jp-modeline-which-function-indicator'.\n  (setq which-func-display 'mode) ; Emacs 30\n  (setq which-func-unknown \"\")\n\n  ;; NOTE 2025-10-24: This is an experiment.  It seems to work, but there may be downsides.\n  (with-eval-after-load 'jp-modeline\n    (defun jp/which-function ()\n      \"A more opinionated `which-function'.\"\n      (let ((name nil))\n        (cond\n         ((derived-mode-p 'lisp-data-mode)\n          (ignore-errors\n            (when-let* ((text (save-excursion\n                          (beginning-of-defun)\n                          (buffer-substring-no-properties (line-beginning-position) (line-end-position))))\n                        (definition (replace-regexp-in-string \"(.+?\\s+\\\\(.*\\\\)\" \"\\\\1\" text)))\n              (setq name (if (string-prefix-p \";\" definition)\n                             \"\"\n                           (jp-modeline-string-abbreviate-but-last definition 1))))))\n         (t\n          (when (null name)\n            (setq name (add-log-current-defun)))\n          ;; If Imenu is loaded, try to make an index alist with it.\n          ;; If `add-log-current-defun' ran and gave nil, accept that.\n          (when (and (null name)\n                     (null add-log-current-defun-function))\n            (when (and (null name)\n                       (boundp 'imenu--index-alist)\n                       (or (null imenu--index-alist)\n                           ;; Update if outdated\n                           (/= (buffer-chars-modified-tick) imenu-menubar-modified-tick))\n                       (null which-function-imenu-failed))\n              (ignore-errors (imenu--make-index-alist t))\n              (unless imenu--index-alist\n                (setq-local which-function-imenu-failed t)))\n            ;; If we have an index alist, use it.\n            (when (and (null name)\n                       (boundp 'imenu--index-alist) imenu--index-alist)\n              (let ((alist imenu--index-alist)\n                    (minoffset (point-max))\n                    offset pair mark imstack namestack)\n                ;; Elements of alist are either (\"name\" . marker), or\n                ;; (\"submenu\" (\"name\" . marker) ... ). The list can be\n                ;; arbitrarily nested.\n                (while (or alist imstack)\n                  (if (null alist)\n                      (setq alist     (car imstack)\n                            namestack (cdr namestack)\n                            imstack   (cdr imstack))\n\n                    (setq pair (car-safe alist)\n                          alist (cdr-safe alist))\n\n                    (cond\n                     ((atom pair))            ; Skip anything not a cons.\n\n                     ((imenu--subalist-p pair)\n                      (setq imstack   (cons alist imstack)\n                            namestack (cons (car pair) namestack)\n                            alist     (cdr pair)))\n\n                     ((or (number-or-marker-p (setq mark (cdr pair)))\n                          (and (overlayp mark)\n                               (setq mark (overlay-start mark))))\n                      (when (and (>= (setq offset (- (point) mark)) 0)\n                                 (< offset minoffset)) ; Find the closest item.\n                        (setq minoffset offset\n                              name (if (null which-func-imenu-joiner-function)\n                                       (car pair)\n                                     (funcall\n                                      which-func-imenu-joiner-function\n                                      (reverse (cons (car pair) namestack)))))))))))))\n          (jp-modeline-string-cut-end name)))))\n\n    (advice-add #'which-function :override #'jp/which-function)\n\n    (which-function-mode 1)))\n\n;;; Keycast mode\n(jp-emacs-configure\n  (jp-emacs-install keycast)\n  (with-eval-after-load 'jp-modeline\n    (setq keycast-mode-line-format \"%2s%k%c%R\")\n    (setq keycast-mode-line-insert-after 'jp-modeline-vc-branch)\n    (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)\n    ; (keycast-mode-line-mode)\n    (setq keycast-mode-line-remove-tail-elements nil))\n  (with-eval-after-load 'keycast\n    (dolist (input '(self-insert-command org-self-insert-command))\n      (add-to-list 'keycast-substitute-alist `(,input \".\" \"Typing…\")))\n\n    (dolist (event '(\"<mouse-event>\" \"<mouse-movement>\" \"<mouse-2>\" \"<drag-mouse-1>\" \"<wheel-up>\" \"<wheel-down>\" \"<double-wheel-up>\" \"<double-wheel-down>\" \"<triple-wheel-up>\" \"<triple-wheel-down>\" \"<wheel-left>\" \"<wheel-right>\" handle-select-window mouse-set-point  mouse-drag-region))\n      (add-to-list 'keycast-substitute-alist `(,event nil nil)))))\n\n(provide 'jp-emacs-modeline)\n"
      0 4
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      4 14
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      14 15 (yank-handler (evil-yank-line-handler nil t) fontified t)
      15 33
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      33 37 (yank-handler (evil-yank-line-handler nil t) fontified t)
      37 44
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      44 46 (yank-handler (evil-yank-line-handler nil t) fontified t)
      46 57
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      57 62 (yank-handler (evil-yank-line-handler nil t) fontified t)
      62 66
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      66 90 (yank-handler (evil-yank-line-handler nil t) fontified t)
      90 92
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      92 101
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      101 104
      (yank-handler (evil-yank-line-handler nil t) fontified t) 104
      108
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      108 151
      (yank-handler (evil-yank-line-handler nil t) fontified t) 151
      153
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      153 162
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      162 165
      (yank-handler (evil-yank-line-handler nil t) fontified t) 165
      169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      169 248
      (yank-handler (evil-yank-line-handler nil t) fontified t) 248
      260
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      260 296
      (yank-handler (evil-yank-line-handler nil t) fontified t) 296
      300
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      300 537
      (yank-handler (evil-yank-line-handler nil t) fontified t) 537
      541
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      541 612
      (yank-handler (evil-yank-line-handler nil t) fontified t) 612
      616
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      616 714
      (yank-handler (evil-yank-line-handler nil t) fontified t) 714
      718
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      718 778
      (yank-handler (evil-yank-line-handler nil t) fontified t) 778
      782
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      782 841
      (yank-handler (evil-yank-line-handler nil t) fontified t) 841
      845
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      845 900
      (yank-handler (evil-yank-line-handler nil t) fontified t) 900
      904
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      904 961
      (yank-handler (evil-yank-line-handler nil t) fontified t) 961
      965
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      965 1013
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1013
      1015
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      1015 1024
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1024 1145
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1145
      1149
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1149 1215
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1215
      1219
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1219 1252
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1252
      1256
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1256 1325
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1325
      1330
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1330 1331
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1331
      1352
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1352 1365
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1365
      1389
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1389 1460
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1460
      1468
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1468 1474
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1474
      1485
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1485 1500
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1500
      1539 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1539 1547
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1547 1553
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1553
      1564
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1564 1575
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1575
      9962 (yank-handler (evil-yank-line-handler nil t) fontified nil))))
