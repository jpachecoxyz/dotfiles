;;; Isearch, occur, grep, and extras (jp-search.el)
(jp-emacs-configure
 (setq search-whitespace-regexp ".*?")
 (setq isearch-lax-whitespace t)
 (setq isearch-regexp-lax-whitespace nil))

(jp-emacs-configure
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(jp-emacs-configure
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(jp-emacs-configure
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (setq isearch-repeat-on-direction-change t))

(jp-emacs-configure
 (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
 (jp-emacs-hook occur-mode-hook (jp-common-truncate-lines-silently hl-line-mode)))

(jp-emacs-configure
 (define-key global-map (kbd "C-.") #'isearch-forward-symbol-at-point) ; easier than M-s . // I also have `jp-simple-mark-sexp' on C-,
 (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
 (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines)
 (jp-emacs-keybind isearch-mode-map
   "C-g" #'isearch-cancel ; instead of `isearch-abort'
   "M-/" #'isearch-complete))

(jp-emacs-configure
 (require 'jp-search)
 (jp-emacs-keybind global-map
   "M-s M-%" #'jp-search-replace-markup ; see `jp-search-markup-replacements'
   "M-s M-<" #'jp-search-isearch-beginning-of-buffer
   "M-s M->" #'jp-search-isearch-end-of-buffer
   "M-s g" #'jp-search-grep
   "M-s u" #'jp-search-occur-urls
   "M-s t" #'jp-search-occur-todo-keywords
   "M-s M-t" #'jp-search-grep-todo-keywords ; With C-u it runs `jp-search-git-grep-todo-keywords'
   "M-s M-T" #'jp-search-git-grep-todo-keywords
   "M-s s" #'jp-search-outline
   "M-s M-o" #'jp-search-occur-outline
   "M-s M-u" #'jp-search-occur-browse-url)
 (jp-emacs-keybind isearch-mode-map
   "<up>" #'jp-search-isearch-repeat-backward
   "<down>" #'jp-search-isearch-repeat-forward
   "<backspace>" #'jp-search-isearch-abort-dwim
   "<C-return>" #'jp-search-isearch-other-end)
 (setq jp-search-outline-regexp-alist
       '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
         (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
         (outline-mode . "^\\*+ +")
         (emacs-news-view-mode . "^\\*+ +")
         (conf-toml-mode . "^\\[")
         (markdown-mode . "^#+ +")))
 (setq jp-search-todo-keywords
       (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
               "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

 (jp-emacs-hook
   jp-search-outline-hook
   (pulsar-recenter-center pulsar-reveal-entry)
   nil
   pulsar))

;;; grep and xref
(jp-emacs-configure
  (setq reb-re-syntax 'read)

  (let ((ripgrep (or (executable-find "rg") (executable-find "ripgrep"))))
    ;; All those have been changed for Emacs 28
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
    (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
    (setq xref-file-name-display 'project-relative)
    (setq xref-search-program (if ripgrep 'ripgrep 'grep))

    (setq grep-save-buffers nil)
    (setq grep-use-headings nil) ; Emacs 30

    (setq grep-program (or ripgrep (executable-find "grep")))
    (setq grep-template
          (if ripgrep
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>")))

  (add-hook 'grep-mode #'jp-common-truncate-lines-silently))

;;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.
(unless (>= emacs-major-version 31)
  (jp-emacs-configure
    (jp-emacs-install wgrep)
    (with-eval-after-load 'grep
      (jp-emacs-keybind grep-mode-map
        "e" #'wgrep-change-to-wgrep-mode
        "C-x C-q" #'wgrep-change-to-wgrep-mode
        "C-c C-c" #'wgrep-finish-edit)
      (setq wgrep-auto-save-buffer t)
      (setq wgrep-change-readonly-file t))))

(provide 'jp-emacs-search)
