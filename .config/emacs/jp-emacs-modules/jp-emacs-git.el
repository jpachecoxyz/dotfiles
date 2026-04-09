;;;; `project'
(jp-emacs-configure
  (jp-emacs-keybind global-map
    "C-x p ." #'project-dired
    "C-x p C-g" #'keyboard-quit
    "C-x p <return>" #'project-dired
    "C-x p <delete>" #'project-forget-project)

  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project")) ; Emacs 29
  (setq project-key-prompt-style t) ; Emacs 30

  (advice-add #'project-switch-project :after #'jp-common-clear-minibuffer-message)

  (autoload #'jp-project-maybe-in-tab "jp-project")
  (autoload #'jp-project-switch "jp-project")
  (define-key project-prefix-map (kbd "p") #'jp-project-maybe-in-tab))

;;;; `diff-mode'
(jp-emacs-configure
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax nil))

;;;; `ediff'
(jp-emacs-configure
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)

  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

  (jp-emacs-autoload
    (jp-ediff-visible-buffers-2
     jp-ediff-visible-buffers-3
     jp-ediff-store-layout
     jp-ediff-restore-layout)
    "jp-ediff")
  ;; The C-x v prefix is for all "version control" commands that are
  ;; already built into Emacs.  It makes sense to extend it for this
  ;; use-case.
  (jp-emacs-keybind global-map
    "C-x v 2" #'jp-ediff-visible-buffers-2
    "C-x v 3" #'jp-ediff-visible-buffers-3)
  (add-hook 'ediff-before-setup-hook #'jp-ediff-store-layout)
  (add-hook 'ediff-quit-hook #'jp-ediff-restore-layout))

;;;; `smerge-mode'
(jp-emacs-configure
  (setq smerge-diff-buffer-name "*smerge-diff*")
  (setq smerge-refine-shadow-cursor nil)) ; Emacs 31

;;; Version control framework (vc.el, vc-git.el, and more)
(jp-emacs-configure
  (setq vc-follow-symlinks t)

  (with-eval-after-load 'vc
    ;; Those offer various types of functionality, such as blaming,
    ;; viewing logs, showing a dedicated buffer with changes to affected
    ;; files.
    (require 'vc-annotate)
    (require 'vc-dir)
    (require 'vc-git)
    (require 'add-log)
    (require 'log-view)

    ;; I only use Git.  If I ever need another, I will include it here.
    ;; This may have an effect on performance, as Emacs will not try to
    ;; check for a bunch of backends.
    (setq vc-handled-backends '(Git))

    (setq vc-dir-save-some-buffers-on-revert t) ; Emacs 31

    ;; This one is for editing commit messages.
    (require 'log-edit)
    (setq log-edit-confirm 'changed)
    (setq log-edit-keep-buffer nil)
    (setq log-edit-require-final-newline t)
    (setq log-edit-setup-add-author nil)

    (jp-emacs-hook
      log-edit-hook
      (log-edit-insert-message-template
       log-edit-maybe-show-diff)
      nil
      log-edit)

    (setq vc-display-failed-async-commands t) ; Emacs 31
    (setq vc-find-revision-no-save t)
    (setq vc-annotate-display-mode 'scale) ; scale to oldest
    ;; I use a different account for git commits
    (setq add-log-mailing-address "info@protesilaos.com")
    (setq add-log-keep-changes-together t)
    (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
    (setq vc-git-log-switches '("--stat"))
    (setq vc-git-print-log-follow t)
    (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
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

    ;; These two are from Emacs 29
    (setq vc-git-log-edit-summary-target-len 50)
    (setq vc-git-log-edit-summary-max-len 70)

    (define-advice vc-push (:around (&rest args) prot)
      (let ((current-window (selected-window)))
        (apply args)
        (select-window current-window)))

    (define-advice vc-pull (:around (&rest args) prot)
      (let ((current-window (selected-window)))
        (apply args)
        (select-window current-window)))

    (defun jp/vc-diff-dwim ()
      "Show diff of buffer against file or against VC history."
      (interactive)
      (if-let* ((buffer (current-buffer))
                (_ (buffer-modified-p buffer)))
          (diff-buffer-with-file buffer)
        (call-interactively #'vc-diff)))

    (defvar jp/vc-git-grep-history nil
      "Minibuffer history for `jp/vc-git-grep'.")

    (defun jp/vc-git-grep (directory regexp)
      "Use `vc-git-grep' with REGEXP in the current root Git DIRECTORY."
      (interactive
       (let ((directory (or (vc-root-dir)
                            (locate-dominating-file "." ".git")
                            (user-error "No VC root available"))))
         (list
          directory
          (read-regexp
           (format "vc-git-grep for REGEXP in `%s': "
                   (propertize directory 'face 'warning))
           nil 'jp/vc-git-grep-history))))
      (vc-git-grep regexp "*" directory))

    ;; NOTE: I override lots of the defaults
    (jp-emacs-keybind global-map
      "C-x v B" #'vc-annotate ; Blame mnemonic
      "C-x v g" #'jp/vc-git-grep ; override original `vc-annotate' key
      "C-x v e" #'vc-ediff
      "C-x v k" #'vc-delete-file ; 'k' for kill==>delete is more common
      "C-x v G" #'vc-log-search  ; git log --grep
      "C-x v t" #'vc-create-tag
      "C-x v c" #'vc-clone ; Emacs 31
      "C-x v d" #'jp/vc-diff-dwim
      "C-x v ." #'vc-dir-root ; `vc-dir-root' is from Emacs 28
      "C-x v <return>" #'vc-dir-root)
    (jp-emacs-keybind vc-dir-mode-map
      "t" #'vc-create-tag
      "I" #'vc-log-incoming
      "O" #'vc-log-outgoing
      "o" #'vc-dir-find-file-other-window
      "d" #'vc-diff ; parallel to D: `vc-root-diff'
      "k" #'vc-dir-delete-file
      "G" #'vc-revert)
    (jp-emacs-keybind vc-git-stash-shared-map
      "a" #'vc-git-stash-apply-at-point
      "c" #'vc-git-stash ; "create" named stash
      "k" #'vc-git-stash-delete-at-point ; symmetry with `vc-dir-delete-file'
      "p" #'vc-git-stash-pop-at-point
      "s" #'vc-git-stash-snapshot)
    (jp-emacs-keybind vc-annotate-mode-map
      "M-q" #'vc-annotate-toggle-annotation-visibility
      "C-c C-c" #'vc-annotate-goto-line
      "<return>" #'vc-annotate-find-revision-at-line)
    (jp-emacs-keybind log-edit-mode-map
      "M-s" nil ; I use M-s for my search commands
      "M-r" nil)
    (jp-emacs-keybind log-view-mode-map
      "<tab>" #'log-view-toggle-entry-display
      "<return>" #'log-view-find-revision
      "s" #'vc-log-search))

  (defun jp/modus-vc-annotate ()
    (modus-themes-with-colors
      (setq vc-annotate-background-mode nil)
      (setq vc-annotate-very-old-color fg-dim)
      (setq vc-annotate-color-map
            `(( 20. . ,red)
              ( 40. . ,red-cooler)
              ( 60. . ,red-warmer)
              ( 80. . ,yellow-warmer)
              (100. . ,yellow)
              (120. . ,yellow-cooler)
              (140. . ,green-warmer)
              (160. . ,green)
              (180. . ,green-cooler)
              (200. . ,cyan-cooler)
              (220. . ,cyan-warmer)
              (240. . ,cyan)
              (260. . ,blue-warmer)
              (280. . ,blue)
              (300. . ,blue-cooler)
              (320. . ,blue-intense)
              (340. . ,magenta-cooler)
              (360. . ,fg-dim)))))

  (when (memq jp-emacs-load-theme-family '(modus ef standard))
    (with-eval-after-load 'vc-annotate
      (jp/modus-vc-annotate)
      (add-hook 'modus-themes-after-load-theme-hook #'jp/modus-vc-annotate))))

;;; Interactive and powerful git front-end (Magit)
(jp-emacs-configure
  (setq transient-show-popup 0.5)

  (jp-emacs-install magit)

  ;; Let `display-buffer-alist' do its job
  (setq magit-display-buffer-function #'display-buffer)

  (define-key global-map (kbd "C-c g") #'magit-status)

  (with-eval-after-load 'magit
    (jp-emacs-keybind magit-mode-map
      "C-w" #'nil
      "M-w" #'nil))

  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicators
        `(;; (magit-fringe-bitmap> . magit-fringe-bitmapv)
          (" ▼" . t))) ; same as `org-ellipsis'

  ;; Show icons for files in the Magit status and other buffers.  This
  ;; is from my jp-icons.el.
  (with-eval-after-load 'jp-icons
    (setq magit-format-file-function
          (lambda (_kind file face &rest _)
            (let ((icon (jp-icons-get-file-icon file)))
              (format "%s %s" icon (propertize file 'font-lock-face face))))))

  (setq magit-diff-refine-hunk t)
  (setq magit-diff-refine-ignore-whitespace t)

  (setq magit-log-auto-more t)

  (setq magit-repository-directories
        '(("~/Git/Projects" . 1)))
  (setq magit-repolist-columns
        `(("Name" 25 ,#'magit-repolist-column-ident)
          ("Version" 15 ,#'magit-repolist-column-version
           ((:sort magit-repolist-version<)))
          ("Unpulled" 10 ,#'magit-repolist-column-unpulled-from-upstream
           ((:help-echo "Upstream changes not in branch")
            (:right-align t)
            (:sort <)))
          ("Unpushed" 10 ,#'magit-repolist-column-unpushed-to-upstream
           ((:help-echo "Local changes not in upstream")
            (:right-align t)
            (:sort <)))
          ("Path" 99 ,#'magit-repolist-column-path)))

  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq git-commit-major-mode #'text-mode))

;;; Git gutter (simple experimental)
(jp-emacs-configure

  (require 'cl-lib)
  (defun jp/goto-next-hunk ()
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

  (defun jp/goto-previous-hunk ()
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


  (defun jp/git-gutter-process-git-diff ()
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


  (defun jp/git-gutter-add-mark (&rest args)
    "Add symbols to the left margin based on Git diff statuses.
   - '+' for added lines (lightgreen)
   - '~' for changed lines (yellowish)
   - '-' for deleted lines (tomato)."
    (interactive)
    (set-window-margins (selected-window) 2 0) ;; change to 1,2,3 if you want more columns
    (remove-overlays (point-min) (point-max) 'jp--git-gutter-overlay t)
    (let ((lines-status (or (jp/git-gutter-process-git-diff) '())))
      (save-excursion
        (dolist (line-status lines-status)
          (let ((line-num (car line-status))
                (status (cdr line-status)))
            (when (and line-num status)
              (goto-char (point-min))
              (forward-line (1- line-num))
              (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))
                (overlay-put overlay 'jp--git-gutter-overlay t)
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

  (defun jp/timed-git-gutter-on()
    (run-at-time 0.1 nil #'jp/git-gutter-add-mark))

  (defun jp/git-gutter-off ()
    "Remove all `jp--git-gutter-overlay' marks and other overlays."
    (interactive)
    (set-window-margins (selected-window) 2 0)
    (remove-overlays (point-min) (point-max) 'jp--git-gutter-overlay t)
    (remove-hook 'find-file-hook #'jp-git-gutter-on)
    (remove-hook 'after-save-hook #'jp/git-gutter-add-mark))

  (defun jp/git-gutter-on ()
    (interactive)
    (jp/git-gutter-add-mark)
    (add-hook 'find-file-hook #'jp/timed-git-gutter-on)
    (add-hook 'after-save-hook #'jp/git-gutter-add-mark))

  (defvar-keymap jp-git-gutter-prefix
  :doc "Git gutter commands."
  "p" #'jp/git-gutter-goto-previous-hunk
  "n" #'jp/git-gutter-goto-next-hunk
  "g" #'jp/git-gutter-on
  "r" #'jp/git-gutter-off)

  (defvar-keymap jp-git-gutter-prefix
    :doc "Git gutter commands."
    "p" #'jp/git-gutter-goto-previous-hunk
    "n" #'jp/git-gutter-goto-next-hunk
    "g" #'jp/git-gutter-on
    "G" #'jp/git-gutter-off)

  (keymap-global-set "C-c g" jp-git-gutter-prefix)
  (add-hook 'after-init-hook #'jp/git-gutter-on))

(provide 'jp-emacs-git)
