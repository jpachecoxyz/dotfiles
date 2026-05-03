
;;; Calendar
(jp-emacs-configure
  (with-eval-after-load 'calendar
    (setq calendar-mark-diary-entries-flag nil)
    (setq calendar-mark-holidays-flag t)
    (setq calendar-mode-line-format nil)
    (setq calendar-time-display-form
          '( 24-hours ":" minutes
             (when time-zone (format "(%s)" time-zone))))
    (setq calendar-week-start-day 1)      ; Monday
    (setq calendar-date-style 'iso)
    (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

    (require 'solar)
    (setq calendar-latitude 35.17         ; Not my actual coordinates
          calendar-longitude 33.36)

    (require 'cal-dst)
    (setq calendar-standard-time-zone-name "+0200")
    (setq calendar-daylight-time-zone-name "+0300")))

(jp-emacs-configure
  (jp-emacs-install
    institution-calendar
    "https://github.com/protesilaos/institution-calendar.git")

  ;; If you want to permanently change what M-x calendar shows, enable
  ;; `institution-calendar-mode'.  Otherwise, use the relevant command:
  ;;
  ;; `institution-calendar-cambridge-university'
  ;; `institution-calendar-oxford-university'
  ;;
  ;; And so on for newly registered institutions.

  ;; Remember to read the manual on how to register your own institution!
  (setopt institution-calendar-entity 'oxford-university)

  (setopt institution-calendar-include-extra-week-numbers t)
  (setopt institution-calendar-include-intermonth-header nil))

;;; Appt (appointment reminders which also integrate with Org agenda)
(jp-emacs-configure
  (setq appt-display-diary nil)
  (setq appt-display-format nil)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil) ; TODO 2023-01-25: t does nothing because I disable `ring-bell-function'?
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)") ; This is for the diary
  (setq appt-message-warning-time 6)

  (with-eval-after-load 'org-agenda
    (appt-activate 1)

    ;; NOTE 2021-12-07: In my `jp-org.el' (see further below), I add
    ;; `org-agenda-to-appt' to various relevant hooks.
    ;;
    ;; Create reminders for tasks with a due date when this file is read.
    (org-agenda-to-appt)))

;;; Org-mode (personal information manager)
(jp-emacs-configure
  (setq org-directory (expand-file-name "~/Documents/Emacs/org/"))
  (setq org-imenu-depth 7)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (shell . t)
                                 (lisp . t)
                                 (sql . t)
                                 (dot . t)
                                 (plantuml . t)
                                 (emacs-lisp . t)))

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))

  (jp-emacs-keybind global-map
    "C-c l" #'org-store-link
    "C-c o" #'org-open-at-point-global
    "C-<return>" #'+org/insert-item-below
    "C-S-<return>" #'+org/insert-item-above)

  (with-eval-after-load 'org
    (jp-emacs-keybind org-mode-map
      ;; I don't like that Org binds one zillion keys, so if I want one
      ;; for something more important, I disable it from here.
      "C-a" nil
      "C-d" nil
      "C-S-d" nil
      "C-'" nil
      "C-," nil
      "M-;" nil
      "<C-return>" nil
      "<C-S-return>" nil
      "C-M-S-<right>" nil
      "C-M-S-<left>" nil
      "C-c ;" nil
      "C-c C-x C-c" nil ; disable `org-columns', which I never use
      "C-c M-l" #'org-insert-last-stored-link
      "C-c C-M-l" #'org-toggle-link-display
      "M-." #'org-edit-special) ; alias for C-c ' (mnenomic is global M-. that goes to source
    (define-key org-src-mode-map (kbd "M-,") #'org-edit-src-exit) ; see M-. above

    (jp-emacs-keybind narrow-map
      "b" #'org-narrow-to-block
      "e" #'org-narrow-to-element
      "s" #'org-narrow-to-subtree)

    (jp-emacs-keybind ctl-x-x-map
      "i" #'jp-org-id-headlines
      "r" #'jp-org-id-headlines-readable
      "h" #'jp-org-ox-html)

    ;; My custom extras, which I use for the agenda and a few other Org features.
    (require 'jp-org)
    (require 'org-tempo)

    (jp-emacs-configure
      (jp-emacs-install ox-hugo)

      (require 'ox-hugo)
      (with-eval-after-load 'ox
        (require 'ox-hugo)))

    (jp-emacs-configure
      (setq org-hugo-base-dir "~/webdev/jpachecoxyz/")

      (defun jp/create-hugo-post ()
        "Create a new Hugo post with metadata."
        (interactive)
        (let* ((title (read-string "Post title: "))
               (description (read-string "Post description: "))
               (tags (read-string "Tags (separated by spaces): "))
               (is-draft (y-or-n-p "Is this a draft? "))
               (slug (replace-regexp-in-string " " "-" (downcase title)))
               (file-name (concat slug ".org"))
               (file-path (expand-file-name
                           file-name
                           "~/webdev/jpachecoxyz/org/posts/"))
               (date (format-time-string "%Y-%m-%d"))
               (draft-string (if is-draft "true" "false")))

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
          (insert (format "#+hugo_draft: %s\n" draft-string))

          (goto-char (point-max))
          (insert "\n")
          (set-buffer-modified-p t)))

      (global-set-key (kbd "C-c n p") #'jp/create-hugo-post))

;;;; general settings
    (setq org-link-frame-setup '((file . find-file)))
    (setq org-ellipsis " ...")
    (setq org-special-ctrl-a/e nil)
    (setq org-special-ctrl-k nil)
    (setq org-M-RET-may-split-line '((default . nil)))
    (setq org-hide-emphasis-markers nil)
    (setq org-hide-macro-markers nil)
    (setq org-hide-leading-stars nil)
    (setq org-cycle-separator-lines 0)
    ;; (setq org-structure-template-alist
    ;;       '(("s" . "src")
    ;;         ("e" . "src emacs-lisp")
    ;;         ("E" . "src emacs-lisp :results value code :lexical t")
    ;;         ("t" . "src emacs-lisp :tangle FILENAME")
    ;;         ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
    ;;         ("x" . "example")
    ;;         ("X" . "export")
    ;;         ("q" . "quote")))
    (setq org-fold-catch-invisible-edits 'show)
    (setq org-yank-folded-subtrees nil)
    (setq org-return-follows-link t)
    (setq org-loop-over-headlines-in-active-region 'start-level)
    (setq org-modules '(ol-info ol-eww))
    (setq org-use-sub-superscripts '{})
    (setq org-insert-heading-respect-content t)
    (setq org-read-date-prefer-future 'time)
    (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-fontify-whole-block-delimiter-line t)
    (setq org-track-ordered-property-with-tag t)
    (setq org-highest-priority ?A)
    (setq org-lowest-priority ?C)
    (setq org-default-priority ?A)
    (setq org-priority-faces nil)
    (setq-default truncate-lines nil)
    (setq org-startup-truncated nil)

    ;; See my `pulsar' package, defined elsewhere in this setup.
    (jp-emacs-hook
      (org-agenda-after-show-hook org-follow-link-hook)
      (pulsar-recenter-center pulsar-reveal-entry)
      nil
      pulsar)))

;;;; `org-indent-mode' and initial folding
(jp-emacs-configure
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local auto-fill-function nil)))
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-adapt-indentation nil) ; No, non, nein, όχι to literal indentation!
  (setq org-indent-indentation-per-level 4)
  (setq org-startup-folded 'content))

;;;; refile, todo
(with-eval-after-load 'org
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "|" "CANCELLED(c@)" "DONE(d!)")))

  (defface jp/org-todo-alternative
    '((t :inherit (italic org-todo)))
    "Face for alternative TODO-type Org keywords.")

  (defface jp/org-done-alternative
    '((t :inherit (italic org-done)))
    "Face for alternative DONE-type Org keywords.")

  (setq org-todo-keyword-faces
        '(("MAYBE" . jp/org-todo-alternative)
          ("CANCELLED" . jp/org-done-alternative)))

  (defface jp/org-tag-coaching
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#004476")
      (((class color) (min-colors 88) (background dark))
       :foreground "#c0d0ef")
      (t :foreground "cyan"))
    "Face for coaching Org tag.")

  (defface jp/org-tag-protasks
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#603f00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#deba66")
      (t :foreground "yellow"))
    "Face for protasks Org tag.")

  (when (eq jp-emacs-load-theme-family 'modus)
    (setq org-tag-faces
          '(("coaching" . jp/org-tag-coaching)
            ("protasks" . jp/org-tag-protasks))))

  (setq org-use-fast-todo-selection 'expert)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t))

;;;; tags
(jp-emacs-configure
  (with-eval-after-load 'org
    (setq org-tag-alist nil)
    (setq org-auto-align-tags t)
    (setq org-tags-column -90)))

;;;; log
(jp-emacs-configure
  (with-eval-after-load 'org
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-log-note-clock-out nil)
    (setq org-log-redeadline 'time)
    (setq org-log-reschedule 'time)))

;;; encrypt text
(jp-emacs-configure
  (with-eval-after-load 'org
    (require 'org-crypt)
    (require 'epa-file)

    ;; Enable epa-file for encryption/decryption of files
    (epa-file-enable)

    (setq epa-file-encrypt-to '("jpacheco@disroot.org"))  ; Replace with your GPG key email

    ;; Configure org-crypt
    (setq org-crypt-tag-matcher "crypt")                    ; Tag used to encrypt entries
    (setq org-crypt-key "jpacheco@disroot.org")          ; Replace with your GPG key email
    (setq org-tags-exclude-from-inheritance '("crypt"))     ; Prevent inheritance of "crypt" tag
    (setq epa-pinentry-mode 'loopback) ;; Ensures passphrase prompt in minibuffer

    ;; Automatically encrypt entries tagged with "crypt" before saving
    (add-hook 'before-save-hook 'org-crypt-use-before-save-magic)

    ;; Define keybindings for manual encryption/decryption
    (define-key org-mode-map (kbd "C-c C-x e") 'org-encrypt-entries)
    (define-key org-mode-map (kbd "C-c C-x d") 'org-decrypt-entries)))

;;;; links
(jp-emacs-configure
  (with-eval-after-load 'org
    (setq org-return-follows-link t)
    (setq org-link-context-for-files t)
    (setq org-link-keep-stored-after-insertion nil)
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))

;;;; code blocks
(jp-emacs-configure
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (setq org-src-window-setup 'current-window)
    (setq org-edit-src-persistent-message nil)
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)
    (setq org-src-tab-acts-natively t)
    (setq org-edit-src-content-indentation 0)))

;;;; export
(jp-emacs-configure
  ;; NOTE 2023-05-20: Must be evaluated before Org is loaded,
  ;; otherwise we have to use the Custom UI.  No thanks!
  (setq org-export-backends '(html texinfo md))

  (with-eval-after-load 'org
    (setq org-export-with-toc t)
    (setq org-export-headline-levels 8)
    (setq org-export-dispatch-use-expert-ui nil)
    (setq org-html-htmlize-output-type nil)
    (setq org-html-head-include-default-style nil)
    (setq org-html-head-include-scripts nil)))

;;;; capture
(jp-emacs-configure
  (define-key global-map (kbd "C-c c") #'org-capture)

  (with-eval-after-load 'org-capture

    (require 'jp-org)

    (setq org-capture-templates
          (let* ((without-time (concat ":PROPERTIES:\n"
                                       ":CAPTURED: %U\n"
                                       ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                                       ":END:\n\n"
                                       "%a\n%?"))
                 (with-time (concat "DEADLINE: %^T\n"
                                    ":PROPERTIES:\n"
                                    ":CAPTURED: %U\n"
                                    ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                                    ":APPT_WARNTIME: 20\n"
                                    ":END:\n\n"
                                    "%a%?")))
            `(("s" "Select file and heading to add to" entry
               (function jp-org-select-heading-in-file)
               ,(concat "* TODO %^{Title}%?\n" without-time)
               :empty-lines-after 1)
              ("t" "Task to do" entry
               (file+headline "~/Documents/Emacs/org/agenda/refile.org" "All tasks")
               ,(concat "* TODO %^{Title} \n" without-time)
               :empty-lines-after 1)
              ;; ("·" "") ; HACK to get a divider
              ("a" "Appointment" entry
               (file+headline "~/Documents/Emacs/org/agenda/refile.org" "Appointments")
               ,(concat "* TODO %^{Title}\n" with-time)
               :empty-lines-after 1))))))

;;;; agenda
(jp-emacs-configure
  ;; I bind `org-agenda' to C-c A, so this one puts me straight into my
  ;; custom block agenda.
  (jp-emacs-keybind global-map
    "C-c A" #'org-agenda
    "C-c a" (lambda ()
              "Call Org agenda with `jp-org-custom-daily-agenda' configuration."
              (interactive)
              (org-agenda nil "a")))

  (with-eval-after-load 'org-agenda
    (jp-emacs-keybind org-agenda-mode-map
      "<tab>" #'org-agenda-next-item
      "<backtab>" #'org-agenda-previous-item)
  (add-hook 'org-agenda-finalize-hook 'org-save-all-org-buffers)

;;;;; Custom agenda blocks

    (require 'jp-org)
    (setq org-agenda-format-date #'jp-org-agenda-format-date-aligned)

    ;; Check the variable `jp-org-custom-daily-agenda' in jp-org.el
    (setq org-agenda-custom-commands
          `(("a" "Daily agenda and top priority tasks"
            ,jp-org-custom-daily-agenda
            ((org-agenda-fontify-priorities nil)
             (org-agenda-files '("~/Documents/Emacs/org/agenda/agenda.org"
                                 "~/Documents/Emacs/org/agenda/bdays.org"
                                 "~/Documents/Emacs/org/agenda/important_dates.org"))
            (org-agenda-dim-blocked-tasks nil)))

            ;; ("p" "Personal Agenda"
            ;;  ,custom-daily-agenda
             ;; ((org-agenda-files '("~/public/org/agenda/personal.org"
            ;;                    "~/public/org/agenda/training.org"))
            ;;   (org-agenda-fontify-priorities nil)
            ;;   (org-agenda-dim-blocked-tasks nil)))

            ;; ("w" "Weekly Review"
            ;; ((agenda ""
            ;;         ((org-agenda-overriding-header "Completed Tasks")
            ;;         (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
            ;;         (org-agenda-span 'week)))
            ;; 
            ;; (agenda ""
            ;;         ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
            ;;         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            ;;         (org-agenda-span 'week)))))
            ;; 
            ;; ("W" "Work Agenda"
            ;; ,jp-org-custom-daily-agenda
            ;; ((org-agenda-files '("~/Documents/Emacs/org/agenda/work.org")
            ;;                     (org-agenda-fontify-priorities nil)
            ;;                     (org-agenda-dim-blocked-tasks nil))))

            ("p" "Planning"
            ((tags-todo "+planning+{@home|@work}"
                        ((org-agenda-overriding-header "Planning Tasks")))

            (tags-todo "-{.*}"
                        ((org-agenda-overriding-header "Unprocessed Tasks")))))

            ;; (todo "-{.*}" ((org-agenda-files '("~/Documents/Emacs/org/agenda/refile.org"))
            ;;             (org-agenda-overriding-header "Unprocessed Items")))))

            ("i" "Important dates"
            ((agenda ""
                    ((org-agenda-overriding-header "Important dates Agenda Overview")
                    (org-agenda-span 'year)
                    (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                    (org-agenda-show-all-dates nil)
                    (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                        'notregexp
                        (regexp-opt '("i-dates"))))))

            (agenda ""
                    ((org-agenda-overriding-header "Upcoming Birthday's")
                    (org-agenda-span 'month)
                    (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                    (org-agenda-start-day "01")
                    (org-agenda-show-all-dates nil)
                    (org-agenda-files '("~/Documents/Emacs/org/agenda/bdays.org"))
                    (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                        'notregexp
                        (regexp-opt '("birthday"))))))))

            ("b" "Birthday Calendar dates"
            ((agenda ""
                    ((org-agenda-overriding-header "Birthday Calendar dates")
                    (org-agenda-span 'year)
                    (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                    (org-agenda-start-day "01")
                    (org-agenda-show-all-dates nil)
                    (org-agenda-files '("~/Documents/Emacs/org/agenda/bdays.org"))
                    (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                        'notregexp
                        (regexp-opt '("birthday"))))))))
            ))

;;;;; Basic agenda setup
    (setq org-default-notes-file (make-temp-file "emacs-org-notes-")) ; send it to oblivion
    (setq org-agenda-files '("~/Documents/Emacs/org/agenda/"))
    (setq org-agenda-span 'week)
    (setq org-agenda-start-on-weekday 1)  ; Monday
    (setq org-agenda-confirm-kill t)
    (setq org-agenda-show-all-dates t)
    (setq org-agenda-show-outline-path nil)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-skip-comment-trees t)
    (setq org-agenda-menu-show-matcher t)
    (setq org-agenda-menu-two-columns nil)
    (setq org-agenda-sticky nil)
    (setq org-agenda-custom-commands-contexts nil)
    (setq org-agenda-max-entries nil)
    (setq org-agenda-max-todos nil)
    (setq org-agenda-max-tags nil)
    (setq org-agenda-max-effort nil)

;;;;; General agenda view options
    ;; ;; NOTE 2021-12-07: Also see my `org-agenda-custom-commands'
    ;; (setq org-agenda-prefix-format
    ;;       '((agenda . " %i %-12:c%?-12t% s")
    ;;         (todo . " %i %-12:c")
    ;;         (tags . " %i %-12:c")
    ;;         (search . " %i %-12:c")))
    (setq org-agenda-prefix-format "%c	 %t %s")
    (setq org-agenda-sorting-strategy
          '(((agenda habit-down time-up priority-down category-keep)
             (todo priority-down category-keep)
             (tags priority-down category-keep)
             (search category-keep))))
    (setq org-agenda-breadcrumbs-separator "->")
    (setq org-agenda-todo-keyword-format "%-1s")
    (setq org-agenda-fontify-priorities 'cookies)
    (setq org-agenda-category-icon-alist nil)
    (setq org-agenda-remove-times-when-in-prefix nil)
    (setq org-agenda-remove-timeranges-from-blocks nil)
    (setq org-agenda-compact-blocks nil)
    (setq org-agenda-block-separator ?—)

;;;;; Agenda marks
    (setq org-agenda-bulk-mark-char "#")
    (setq org-agenda-persistent-marks nil)

;;;;; Agenda follow mode
    (setq org-agenda-start-with-follow-mode nil)
    (setq org-agenda-follow-indirect t)

;;;;; Agenda multi-item tasks
    (setq org-agenda-dim-blocked-tasks t)
    (setq org-agenda-todo-list-sublevels t)

;;;;; Agenda filters and restricted views
    (setq org-agenda-persistent-filter nil)
    (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;; Agenda items with deadline and scheduled timestamps
    (setq org-agenda-include-deadlines t)
    (setq org-deadline-warning-days 0)
    (setq org-agenda-skip-scheduled-if-done nil)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
    (setq org-agenda-skip-deadline-if-done nil)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
    (setq org-agenda-skip-scheduled-delay-if-deadline nil)
    (setq org-agenda-skip-additional-timestamps-same-entry nil)
    (setq org-agenda-skip-timestamp-if-done nil)
    (setq org-agenda-search-headline-for-time nil)
    (setq org-scheduled-past-days 365)
    (setq org-deadline-past-days 365)
    (setq org-agenda-move-date-from-past-immediately-to-today t)
    (setq org-agenda-show-future-repeats t)
    (setq org-agenda-prefer-last-repeat nil)
    (setq org-agenda-timerange-leaders
          '("" "(%d/%d): "))
    (setq org-agenda-scheduled-leaders
          '("Scheduled: " "Sched.%2dx: "))
    (setq org-agenda-inactive-leader "[")
    (setq org-agenda-deadline-leaders
          '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
    ;; Time grid
    (setq org-agenda-time-leading-zero t)
    (setq org-agenda-timegrid-use-ampm nil)
    (setq org-agenda-use-time-grid t)
    (setq org-agenda-show-current-time-in-grid t)
    (setq org-agenda-current-time-string (concat "Now " (make-string 70 ?.)))
    (setq org-agenda-time-grid
          '((daily today require-timed)
            ( 0500 0600 0700 0800 0900 1000
              1100 1200 1300 1400 1500 1600
              1700 1800 1900 2000 2100 2200)
            "" ""))
    (setq org-agenda-default-appointment-duration nil)

;;;;; Agenda global to-do list
    (setq org-agenda-todo-ignore-with-date t)
    (setq org-agenda-todo-ignore-timestamp t)
    (setq org-agenda-todo-ignore-scheduled t)
    (setq org-agenda-todo-ignore-deadlines t)
    (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
    (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;; Agenda tagged items
    (setq org-agenda-show-inherited-tags t)
    (setq org-agenda-use-tag-inheritance
          '(todo search agenda))
    (setq org-agenda-hide-tags-regexp nil)
    (setq org-agenda-remove-tags nil)
    (setq org-agenda-tags-column 1)

;;;;; Agenda entry
    ;; NOTE: I do not use this right now.  Leaving everything to its
    ;; default value.
    (setq org-agenda-start-with-entry-text-mode nil)
    (setq org-agenda-entry-text-maxlines 5)
    (setq org-agenda-entry-text-exclude-regexps nil)
    (setq org-agenda-entry-text-leaders "    > ")

;;;;; Agenda logging and clocking
    ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
    ;; to its default value for the time being.
    (setq org-agenda-log-mode-items '(closed clock))
    (setq org-agenda-clock-consistency-checks
          '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                           ("4:00")
                           :default-face ; This should definitely be reviewed
                           ((:background "DarkRed")
                            (:foreground "white"))
                           :overlap-face nil :gap-face nil :no-end-time-face nil
                           :long-face nil :short-face nil)))
    (setq org-agenda-log-mode-add-notes t)
    (setq org-agenda-start-with-log-mode nil)
    (setq org-agenda-start-with-clockreport-mode nil)
    (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
    (setq org-agenda-search-view-always-boolean nil)
    (setq org-agenda-search-view-force-full-words nil)
    (setq org-agenda-search-view-max-outline-level 0)
    (setq org-agenda-search-headline-for-time t)
    (setq org-agenda-use-time-grid t)
    (setq org-agenda-cmp-user-defined nil)
    (setq org-agenda-sort-notime-is-late t) ; Org 9.4
    (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

    ;; ;;;;; Agenda habits
    ;;   (require 'org-habit)
    ;;   (setq org-habit-graph-column 50)
    ;;   (setq org-habit-preceding-days 9)
    ;;   ;; Always show the habit graph, even if there are no habits for
    ;;   ;; today.
    ;;   (setq org-habit-show-all-today t)

;;;;; Agenda column view
    ;; NOTE I do not use these, but may need them in the future.
    (setq org-agenda-view-columns-initially nil)
    (setq org-agenda-columns-show-summaries t)
    (setq org-agenda-columns-compute-summary-properties t)
    (setq org-agenda-columns-add-appointments-to-effort-sum nil)
    (setq org-agenda-auto-exclude-function nil)
    (setq org-agenda-bulk-custom-functions nil)))

;;; Page break lines
(jp-emacs-configure
  (jp-emacs-install page-break-lines)

  (add-hook 'org-mode-hook #'page-break-lines-mode))

;;; Custom org tweaks
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;;###autoload
(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))


(defun org-make-olist (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (insert (concat (number-to-string (1+ i)) ". "))
      (forward-line))))

;;;; org-id
(declare-function org-id-add-location "org")
(declare-function org-with-point-at "org")
(declare-function org-entry-get "org")
(declare-function org-id-new "org")
(declare-function org-entry-put "org")

;; Copied from this article (with minor tweaks from my side):
;; https://writequit.org/articles/emacs-org-mode-generate-ids.html.
(defun jp/org--id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match \\S- id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
        id)))))

(declare-function org-map-entries "org")

;;;###autoload
(defun jp/org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (jp/org--id-get (point) t))))

;;;###autoload
(defun jp/org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (jp/org--id-get (point) t))

(defun jp/org-id-store-link-for-headers ()
  "Run `org-id-store-link' for each header in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (org-id-store-link))))

(defun jp/org-toggle-emphasis-markers (&optional arg)
  "Toggle visibility of Org emphasis markers."
  (interactive "p")
  (setq-local org-hide-emphasis-markers
              (not org-hide-emphasis-markers))
  (message "Emphasis markers are now %s."
           (if org-hide-emphasis-markers "hidden" "visible"))
  (when arg
    (font-lock-fontify-buffer)))

(defun jp/org-hide-emphasis-markers (&optional arg)
  "Hide Org emphasis markers."
  (interactive "p")
  (setq-local org-hide-emphasis-markers t)
  ;; (message "Emphasis markers are now hidden.")
  (when arg
    (font-lock-fontify-buffer)))

(defun jp/org-show-emphasis-markers (&optional arg)
  "Show Org emphasis markers."
  (interactive "p")
  (setq-local org-hide-emphasis-markers nil)
  ;; (message "Emphasis markers are now visible.")
  (when arg
    (font-lock-fontify-buffer)))

(add-hook 'org-mode-hook #'jp/org-hide-emphasis-markers)

(defun export-org-email ()
  "Export the current email org buffer and copy it to the
clipboard"
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil)
        (org-html-head (org-email-html-head)))
    (org-html-export-as-html)
    (with-current-buffer "*Org HTML Export*"
      (kill-new (buffer-string)))
    (message "HTML copied to clipboard")))
(global-set-key (kbd "C-c C-x C-e") 'export-org-email)

(defun org-email-html-head ()
  "Create the header with CSS for use with email"
  (concat
   "<style type=\"text/css\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents
      "~/.emacs.d/src/css/org2outlook.css")
     (buffer-string))
   "/*]]>*/-->\n"
   "</style>\n"))

(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "DOING")))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "DOING")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "DOING")))
                (org-todo-if-needed "DOING"))))))))
(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)

;;; Org-msg
(jp-emacs-configure
  (jp-emacs-install org-msg)
  (setq mail-user-agent 'mu4e-user-agent)
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars indent inlineimages"
	    org-msg-greeting-fmt "\nHello%s,\n\n"
	    ;; org-msg-recipient-names '(("jeremy.compostella@gmail.com" . "Jérémy"))
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		. (text html))
				                       (reply-to-html	. (text html))
				                       (reply-to-text	. (text)))
	    org-msg-convert-citation t
	    org-msg-signature "

Regards,

 #+begin_signature
 --
 *Ing. Javier Pacheco*
 /\"The best way to predict the future is to invent it.\"/
 https://jpachecoxyz.github.io
 #+end_signature")
  (org-msg-mode))
(provide 'jp-emacs-org)
