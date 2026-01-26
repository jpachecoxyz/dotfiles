;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; Load Utilities.el.
(load! "utilities")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Javier Pacheco"
      user-mail-address "jpacheco@disroot.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

(remove-hook! '(text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq initial-buffer-choice 'vterm)
(setq confirm-kill-emacs nil)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(94 . 95))
(add-to-list 'default-frame-alist '(alpha . (94 . 95)))

;; Maximized frame
(toggle-frame-maximized)

;; Blink cursor
(blink-cursor-mode 1)

;;hl-line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
                    :extend t)           ;; que se extienda más allá del texto
                    ;; :background "#3a3a3a")

;; Desactivar preguntas por *cualquier* proceso vivo al salir
(setq confirm-kill-processes nil)

(require 'org-crypt)

(setq epa-file-encrypt-to '("jpacheco@disroot.org"))
(setq org-crypt-tag-matcher "crypt")
(setq org-crypt-key "jpacheco@disroot.org")
(setq org-tags-exclude-from-inheritance '("crypt"))
(setq epa-pinentry-mode 'loopback)

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      #'org-crypt-use-before-save-magic
                      nil t)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x e") #'org-encrypt-entries)
  (define-key org-mode-map (kbd "C-c C-x d") #'org-decrypt-entries))

(map! :leader :desc "Open my most used files" "ef" #'open-specific-dired)
(map! :leader :desc "Open the scratch buffer" "os" #'toggle-scratch-buffer)
;; (map! :leader :desc "Open the org buffer" "oo" #'toggle-org-buffer)
(map! :leader :desc "Pass consult" "op" #'+pass/consult)
(map! :leader :desc "Org agenda" "a" #'org-agenda)

(setq-default fill-column 80) ;; Column 80
(setq global-display-fill-column-indicator-mode nil)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'org-mode-hook #'display-fill-column-indicator-mode)


(use-package! rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package! rainbow-mode
  :hook ((org-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

(use-package! rapid-mode
  :mode (("\\.MOD\\'" . rapid-mode)
         ("\\.sys\\'" . rapid-mode)))

(add-to-list 'display-buffer-alist
             '("\\*typst-ts-compilation\\*"
               (display-buffer-no-window)))

(add-to-list 'display-buffer-alist
             '("\\*Org Src.*\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.45)
               (dedicated . t)))

(setq doom-font (font-spec :family "CaskaydiaMono Nerd Font" :size 17 :weight 'regular)
     doom-variable-pitch-font (font-spec :family "CaskaydiaMono Nerd Font" :size 17))

(custom-theme-set-faces!
'doom-opera
'(org-document-title :bold t :underline nil)
'(show-paren-match :bold t  :background "#1b1b1b")
'(hl-line :extend t :background "#3a3a3a"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Emacs/org/agenda")

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (c . t)))

(setq org-hide-emphasis-markers t)

(defvar jp/org-contact-template-personal
  "** %^{Name}
:PROPERTIES:
:PHONE: %^{Phone: 123-456-7890}
:EMAIL: %^{Email}
:BIRTHDAY: %^{Birthday}t
:END:"
  "Org template for personal contacts.")

(defvar jp/org-contact-template-company
  "** %^{Company Name}
:PROPERTIES:
:MODEL: %^{Business Model|Supplier|Integrator|Service Provider|OEM}
:LINE:  %^{Technical Line|Automation & Robotics|Industrial Maintenance|Utilities & HVAC|Tooling & Machining|Die Casting|Furnaces}
:END:"
  "Org template for companies contacts.")

(defvar jp/org-contact-template-professional
  "*** %^{Name}
:PROPERTIES:
:PHONE: %^{Phone: 123-456-7890}
:EMAIL: %^{Email}
:END:"
  "Org template for professional contacts.")

;; -------------------------
;; Contacts file
;; -------------------------
(defvar jp/contacts-file "~/Documents/Emacs/org/agenda/contacts.org"
  "Main contacts Org file.")

(global-set-key (kbd "C-c c") 'org-capture)      ;; use C-c c to start capture mode

;; capture templates for: TODO tasks, Notes, appointments, meetings
(setq org-templates-location-var (concat org-directory "agenda/refile.org"))

;; My personal contatcs.org file
(defvar jp/contacts-file "~/Documents/Emacs/org/agenda/contacts.org"
  "Main contacts Org file.")

(defvar jp/org-refile-file
  "~/Documents/Emacs/org/agenda/refile.org"
  "Main refill Org file.")

(defvar jp/org-capture-template-scheduled
  "** [#A] %?
SCHEDULED: %^t"
  "Capture template for scheduled priority tasks.")

(defvar jp/org-capture-template-deadline
  "** %?
DEADLINE: %^t"
  "Capture template for deadline tasks.")

(defvar jp/org-capture-template-note
  "** %?"
  "Capture template for notes.")

(setq org-capture-templates
      `(
        ("s" "Scheduled Task"
         entry
         (file+headline ,jp/org-refile-file "Priority")
         ,jp/org-capture-template-scheduled
         :empty-lines 1)

        ("d" "Deadline"
         entry
         (file+headline ,jp/org-refile-file "Deadline")
         ,jp/org-capture-template-deadline
         :empty-lines 1)

        ("p" "Personal contact"
         entry
         (file+headline ,jp/contacts-file "Personal")
         ,jp/org-contact-template-personal
         :jump-to-captured nil)

        ;; ---- CONTACTS DISPATCHER ----
        ("P" "Professional Contacts")

        ("Pc" "Company contact"
         entry
         (file+headline ,jp/contacts-file "Companies")
         ,jp/org-contact-template-company
         :jump-to-captured nil)

        ("Pp" "Professional contact"
         entry
         (file+headline ,jp/org-refile-file "Contacts")
         ,jp/org-contact-template-professional
         :jump-to-captured nil)
        ))

;; Refile
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; C-c C-w for refile
(setq org-refile-targets
      `((org-agenda-files :maxlevel . 2)
        (,jp/contacts-file :maxlevel . 2)))

(setq org-tag-alist
      '((:startgroup . "Context")
        ("@home" . ?h)
        ("@work" . ?w)
        ("@personal" . ?p)
        (:endgroup)
        
        (:startgroup . "Difficulty")
        ("easy" . ?E)
        ("medium" . ?M)
        ("challenging" . ?C)
        (:endgroup)
        
        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?P)
        ("@writing" . ?W)
        ("@email" . ?e)
        ("@calls" . ?c)
        ("@errands" . ?r)))
(setq org-agenda-tags-column 90)

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      ;; Unbind the keys
      :m "/" nil
      :m "<tab>" nil
      :m "<backtab>" nil

      ;; Set the binds
      :m "/" #'org-agenda-filter
      :m "<tab>" #'org-agenda-next-item
      :m "<backtab>" #'org-agenda-previous-item)

(setq org-agenda-custom-commands
      `(("w" "Main Agenda"
        (
        ;; ─────────────────────────────────────────────
        ;; Main Agenda Overview (TODOs, NOT DOING)
        ;; ─────────────────────────────────────────────
        (tags-todo "+@home|@work"
                ((org-agenda-span 'week)
                (org-agenda-start-on-weekday 1)
                (org-agenda-block-separator ?-)
                (org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'todo '("DOING" "DONE")))
                (org-agenda-overriding-header "Main Agenda Overview")))

        ;; ─────────────────────────────────────────────
        ;; Tasks in DOING (estado DOING REAL)
        ;; ─────────────────────────────────────────────
        (todo "DOING"
                ((org-agenda-span 'week)
                (org-agenda-start-on-weekday 1)
                (org-agenda-block-separator ?-)
                (org-agenda-overriding-header "Tasks in DOING")))


        ;; ─────────────────────────────────────────────
        ;; Today's agenda
        ;; ─────────────────────────────────────────────
        (agenda ""
                ((org-agenda-span 0)
                (org-agenda-start-day "0d")
                (org-agenda-show-log nil)
                (org-agenda-block-separator ?-)
                (org-agenda-overriding-header "Today's agenda")))

        ;; ─────────────────────────────────────────────
        ;; Upcoming tasks (+14d) — solo SCHEDULED
        ;; ─────────────────────────────────────────────
        (agenda ""
                ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; (org-agenda-start-day "+6d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-entry-types '(:scheduled :deadline))
                (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-block-separator ?-)
                (org-agenda-overriding-header
                "Upcoming tasks (+14d)")))))
 
        ("S" "Scheduled & Deadlines"
         ((agenda ""
                  ((org-agenda-overriding-header "📅 Scheduled & Deadlines")
                   (org-agenda-span 14)
                   (org-agenda-entry-types '(:scheduled :deadline))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED"))
                   (org-agenda-time-grid nil)
                   (org-agenda-block-separator nil)))))
 
        ("s" "Scheduled"
         ((agenda ""
                  ((org-agenda-overriding-header "🗓 Scheduled")
                   (org-agenda-span 14)
                   (org-agenda-entry-types '(:scheduled))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-time-grid nil)
                   (org-agenda-block-separator ?-)))))
 
        ("d" "Deadlines"
         ((agenda ""
                  ((org-agenda-overriding-header "⏰ Deadlines")
                   (org-agenda-span 14)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-deadline-warning-days 14)
                   (org-agenda-time-grid nil)
                   (org-agenda-block-separator ?-)))))

        ("A" "All Tasks"
        ((agenda ""
            ((org-agenda-overriding-header "Completed Tasks\n")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
            (org-agenda-block-separator ?-)
            (org-agenda-span 'week)))

        (agenda ""
            ((org-agenda-overriding-header "Unfinished Scheduled Tasks\n")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-block-separator ?-)
            (org-agenda-span 'week)))))

        ("k" "Kanban Dashboard"
         ((todo "TODO"
                ((org-agenda-overriding-header "🟥 TODO")
                 (org-agenda-block-separator ?-)
                 (org-agenda-sorting-strategy '(priority-down effort-up))))

          (todo "DOING"
                ((org-agenda-overriding-header "🟨 DOING")
                 (org-agenda-block-separator ?-)
                 (org-agenda-sorting-strategy '(priority-down effort-up))))

          (todo "DONE"
                ((org-agenda-overriding-header "🟩 DONE")
                 (org-agenda-block-separator ?-)
                 (org-agenda-sorting-strategy '(priority-down effort-up))))))
        
        ("p" "Planning"
        ((tags-todo "+planning+@home|@work"
                ((org-agenda-overriding-header "Planning Tasks\n")))

        (todo ".*" ((org-agenda-files '("~/Documents/Emacs/org/agenda/refile.org"))
                (org-agenda-overriding-header "Unprocessed refill.org Items")))))

        ("i" "Important dates"
        ((agenda ""
                ((org-agenda-overriding-header "Important dates Agenda Overview\n")
                (org-agenda-span 'year)
                (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                (org-agenda-show-all-dates nil)
                (org-agenda-block-separator ?-)
                        (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                                'notregexp
                                (regexp-opt '("i-dates"))))))

        (agenda ""
                ((org-agenda-overriding-header "Upcoming Birthday's\n")
                (org-agenda-span 'month)
                (org-agenda-start-on-weekday 0) ;; Start the week on Sunday
                (org-agenda-start-day "01")
                (org-agenda-show-all-dates nil)
                (org-agenda-files '("~/Documents/Emacs/org/agenda/bdays.org"))
                (org-agenda-block-separator ?-)
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
                (org-agenda-block-separator ?-)
                (org-agenda-skip-function
                '(org-agenda-skip-entry-if
                        'notregexp
                        (regexp-opt '("birthday"))))))))
		
		))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-track-ordered-property-with-tag t)
(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)

;; Server Mode

(use-package! server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;; Web jump
(use-package! webjump
  :defer t
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/results?search_query=" ""])
     ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

;;; EMACS-JP-OLIVETTI
;;
(use-package! emacs-jp-olivetti
  ;; :if emacs-jp-enable-olivetti
  :no-require t
  :defer t
  :init
  (defvar emacs-jp-center-document-desired-width 120
    "The desired width of a document centered in the window.")

  (defun emacs-jp/center-document--adjust-margins ()
    ;; Reset margins first before recalculating
    (set-window-parameter nil 'min-margins nil)
    (set-window-margins nil nil)

    ;; Adjust margins if the mode is on
    (when emacs-jp/center-document-mode
      (let ((margin-width (max 0
                               (truncate
                                (/ (- (window-width)
                                      emacs-jp-center-document-desired-width)
                                   2.0)))))
        (when (> margin-width 0)
          (set-window-parameter nil 'min-margins '(0 . 0))
          (set-window-margins nil margin-width margin-width)))))

  (define-minor-mode emacs-jp/center-document-mode
    "Toggle centered text layout in the current buffer."
    :lighter " Centered"
    :group 'editing
    (if emacs-jp/center-document-mode
        (add-hook 'window-configuration-change-hook #'emacs-jp/center-document--adjust-margins 'append 'local)
      (remove-hook 'window-configuration-change-hook #'emacs-jp/center-document--adjust-margins 'local))
    (emacs-jp/center-document--adjust-margins))


  ;; (add-hook 'org-mode-hook #'emacs-jp/center-document-mode)
  ;; (add-hook 'gnus-group-mode-hook #'emacs-jp/center-document-mode)
  ;; (add-hook 'gnus-summary-mode-hook #'emacs-jp/center-document-mode)
  ;; (add-hook 'gnus-article-mode-hook #'emacs-jp/center-document-mode)
  ;; (add-hook 'org-social-ui-mode-hook #'emacs-jp/center-document-mode)

  ;; (add-hook 'newsticker-treeview-list-mode-hook 'emacs-jp/timed-center-visual-fill-on)
  ;; (add-hook 'newsticker-treeview-item-mode-hook 'emacs-jp/timed-center-visual-fill-on)

  :bind ("<f1>" . #'emacs-jp/center-document-mode))

;;; EMACS-JP-0x0
(use-package! emacs-jp-0x0
  :no-require t
  :defer t
  :init
  (defun emacs-jp/0x0-upload-text ()
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

  (defun emacs-jp/0x0-upload-file (file-path)
    (interactive "fSelect a file to upload: ")
    (message "Sending %s to 0x0.st..." file-path)
    (let ((url (string-trim-right
                (shell-command-to-string
                 (format "curl -s -F'file=@%s' https://0x0.st" (expand-file-name file-path))))))
      (message "The URL is %s" url)
      (kill-new url))))

;;; DENOTE
(use-package! denote
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
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(setq denote-directory (expand-file-name "~/Documents/Emacs/notes"))
(setq denote-dired-directories-include-subdirectories t)
(setq denote-known-keywords '("estudio" "trabajo" "emacs" "linux"))
(setq denote-title-history nil)
(setq denote-sort-keywords nil)
(setq denote-files-matching-regexp-history nil)
(setq denote-history-completion-in-prompts nil)
(setq denote-infer-keywords t)
(setq denote-org-front-matter "# -*- jinx-languages: \"es_MX\"; -*-\n#+title: %s\n#+date: %s\n#+filetags: %s\n#+identifier: %s\n#+author: Ing. Javier Pacheco\n#+startup: showall\n")
(setq denote-query-links-display-buffer-action
      '((display-buffer-same-window)))
(setq denote-link--prepare-links-format "%s\n")

(defun jp:denote-dired-open()
  "Open `denote-directory` in Dired and filter only notes matching proper Denote filename pattern."
  (interactive)
  (dired denote-directory)
  ;; Disable diredfl only for this Denote Dired buffer
  (when (fboundp 'diredfl-mode)
    (diredfl-mode -1))

  ;; Optional: mark buffer as Denote Dired (informational)
  (denote-dired-mode t)
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
  (let* ((img-dir (expand-file-name "~/Documents/Emacs/notes/img/"))
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
  (let* ((pdf-dir (expand-file-name "~/Documents/Emacs/pdf"))
         (pdf-files (directory-files-recursively pdf-dir "\\.pdf$"))
         (chosen-file (completing-read "Choose a PDF: " pdf-files nil t))
         (custom-title (read-string "Enter a link title: ")))
    (insert (format "- [[file:%s][%s]]"
                    (file-relative-name chosen-file)
                    custom-title))))

(use-package! denote-silo
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
              "~/Documents/Emacs/notes/inbox/")))

(use-package! denote-sequence
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

(use-package! denote-menu
  :custom ((denote-menu-show-file-type nil)
           (denote-menu-initial-regex "==[0-9]+--.*_meta.org"))
  :bind
  (("C-c n m" . denote-menu-list-notes)))

(use-package! denote-explore
  :bind* (("C-c e n" . denote-explore-network)
        ("C-c e v" . denote-explore-network-regenerate)
        ("C-c e D" . denote-explore-barchart-degree)))
(setq denote-explore-network-d3-template "~/.dotfiles/.emacs.d/explore.html")

(use-package! tmr
  :config
  (define-key global-map (kbd "C-c t") #'tmr-prefix-map)
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history))

;; (use-package ox-typst
;;   :ensure t
;;   :after ox)

(use-package! typst-ts-mode
  :custom
  (typst-ts-watch-options "--open")
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

(use-package! treesit-auto
  :ensure t
  :custom (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

(use-package! treesit-ispell
  :ensure t
  :defer t
  :bind (("C-x C-s" . treesit-ispell-run-at-point)))

(with-eval-after-load 'treesit
  (setq treesit-font-lock-level 4))

(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "~/.dotfiles/.emacs.d/lang/en_US.aff")
        ("es_MX" "~/.dotfiles/.emacs.d/lang/es_MX.aff")))

    ;;; Linux
(setq ispell-local-dictionary-alist
    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
        ("es_MX" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
        ))

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")

;; ;; Change betwen English and Spanish,
;; ;; English is he default.
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

;; (global-set-key (kbd "<f8>") 'toggle-ispell-dictionary)

(use-package! jinx
:hook (text-mode . jinx-mode)
:bind (("M-;" . jinx-correct)
        ("<f8>" . jinx-languages)))
(add-hook 'text-mode-hook #'jinx-mode)

(dolist (hook '(text-mode-hook conf-mode-hook))
  (add-hook hook #'jinx-mode))

(use-package! ox-hugo
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
         (file-path (expand-file-name file-name "~/webdev/jpachecoxyz//org/posts/"))
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
    (goto-char (point-max)) (insert "\n") ;; Ensure a blank line before the cursor
    (set-buffer-modified-p t)))

(global-set-key (kbd "C-c n p") #'jp:create-hugo-post)

(use-package! fzf
  :config
  (setq
   fzf/args
   "--color=fg:-1,fg+:#d0d0d0,bg:-1,bg+:#282828 --color=hl:#5f87af,hl+:#5fd7ff,info:#afaf87,marker:#87ff00 --color=prompt:#458588,spinner:#af5fff,pointer:#af5fff,header:#87afaf --color=gutter:-1,border:#262626,label:#aeaeae,query:#d9d9d9 --border='bold' --border-label='' --preview-window='border-bold' --prompt='❯❯ ' --marker='*' --pointer='->' --separator='─' --scrollbar='│' --layout='reverse-list' --info='right' --height 30 --preview 'bat --style=numbers --color=always --line-range :500 {}' "

   fzf/executable "fzf"
   fzf/git-grep-args "-i --line-number %s"
   ;; command used for `fzf-grep-*` functions
   ;; example usage for ripgrep:
   ;; fzf/grep-command "rg --no-heading -nH"
   fzf/grep-command "grep -nrH"
   ;; If nil, the fzf buffer will appear at the top of the window
   fzf/position-bottom t
   fzf/window-height 30))

(use-package! mermaid-mode)

(setq org-babel-mermaid-cli-path "/usr/local/bin/mmdc")
;; (setenv "PUPPETEER_EXECUTABLE_PATH" "/usr/bin/chromium")

(setq ob-mermaid-cli-path "mmdc")
(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)
      (c . t)))

(use-package! mu4e 
  :config

  ;; --- Mail sync ---
  ;; refresca cada 10 minutos
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")

  ;; Maildir raíz
  (setq mu4e-maildir (expand-file-name "~/Mail"))

  ;; --- auth-source / pass ---
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-debug nil)
  (setq auth-source-do-cache nil)

  ;; --- Composición ---
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  (setq message-kill-buffer-on-exit t)

  ;; Evita errores al mover correos
  (setq mu4e-change-filenames-when-moving t)

  ;; Envío de correo
  ;; (setq message-send-mail-function 'smtpmail-send-it)
  (setq sendmail-program "msmtp"
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

  ;; --- Visual ---
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)

  ;; Contextos
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)
  (setq mu4e-confirm-quit nil)

  ;; --- CONTEXTO DISROOT ---
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Disroot"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/disroot"
                               (mu4e-message-field msg :maildir))))
          :vars
          '((user-mail-address . "jpacheco@disroot.org")
            (user-full-name    . "Javier Pacheco")

            ;; SMTP (msmtp / smtpmail)
            (smtpmail-smtp-server  . "smtp.disroot.org")
            (smtpmail-smtp-service . 587)
            (smtpmail-stream-type  . starttls)
            (smtpmail-smtp-user    . "jpacheco@disroot.org")

            ;; Carpetas
            (mu4e-drafts-folder  . "/disroot/Drafts")
            (mu4e-sent-folder    . "/disroot/Sent")
            (mu4e-refile-folder  . "/disroot/Archive")
            (mu4e-trash-folder   . "/disroot/Trash")

            ;; Firma
            (mu4e-compose-signature .
             "Javier Pacheco\nhttps://jpachecoxyz.github.io")))))

  ;; --- Fast Inbox ---
  (setq m/mu4e-inbox-query
        "(maildir:/disroot/INBOX) AND flag:unread")
  
 ;; ---  Shortcuts ---
  (setq mu4e-maildir-shortcuts
    '((:maildir "/disroot/INBOX"     :key ?i)
      (:maildir "/disroot/Sent"      :key ?s)
      (:maildir "/disroot/Trash"     :key ?t)
      (:maildir "/disroot/Drafts"    :key ?d)))
  
  ;; --- Bookmarks
  (setq mu4e-bookmarks
  '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?i)
      (:name "Today's messages" :query "date:today..now" :key ?t)
      ;; (:name "The Boss" :query "from:stallman" :key ?s)
      (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
      (:name "Messages with images" :query "mime:image/*" :key ?p)))

  (defun m/go-to-inbox ()
    (interactive)
    (mu4e-headers-search m/mu4e-inbox-query)))
  ;; (mu4e t))

(use-package! pdf-tools
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-page-command)
              ("k" . pdf-view-previous-page-command))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pf-view-mode-hook (blink-cursor-mode -1))

(defun my-evil-pdf-view-keybindings ()
  (evil-define-key 'normal doc-view-mode-map
    "j" 'pdf-view-next-page-command
    "k" 'pdf-view-previous-page-command))

(add-hook 'pdf-view-mode-hook 'my-evil-pdf-view-keybindings)

(use-package! doc-view
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

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 75))

(use-package! hydra
  :bind (("C-c g" . hydra-go-to-file/body)
         ("C-c o" . hydra-org/body)
         ("C-c w" . hydra-windows/body)))

(use-package! major-mode-hydra
  :after hydra)

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title "Go To")
  ("Agenda"
   (("ac" (find-file "~/Documents/Emacs/org/agenda/contacts.org") "contacts")
    ("aa" (find-file "~/Documents/Emacs/org/agenda/agenda.org") "agenda")
    ("ar" (find-file "~/Documents/Emacs/org/agenda/refile.org") "refile")
    ("aw" (find-file "~/Documents/Emacs/org/agenda/work.org") "work agenda"))
   "Config"
   (("ce" (find-file "~/.dotfiles/.config/doom/config.org") "doom emacs"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title "Org")
  ("Action"
   (("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" org-capture-goto-last-stored "jump-capture")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

(pretty-hydra-define hydra-windows
  (:hint nil :foreign-keys warn :quit-key "q" :title "Windows")
  ("Window"
   (("b" balance-windows "balance")
    ("c" centered-window-mode "center")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("u" winner-undo "undo")
    ("r" winner-redo "redo")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))
