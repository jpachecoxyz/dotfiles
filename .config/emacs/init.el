
;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load: search below for the files
;; jp-emacs-pre-custom.el and jp-emacs-post-custom.el

(eval-when-compile
  (require 'cl-lib))

(defgroup jp-emacs nil
  "User options for my dotemacs.
These produce the expected results only when set in a file called
jp-emacs-pre-custom.el.  This file must be in the same
directory as the init.el."
  :group 'file)

(defcustom jp-emacs-load-theme-family 'ef
 "Set of themes to load.
Valid values are the symbols `doric', `ef', `modus', and `standard',
which reference the `doric-themes', `ef-themes', `modus-themes', and
`standard-themes', respectively.

A nil value does not load any of the above (use Emacs without a
theme).

This user option must be set in the `jp-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'jp-emacs
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `doric-themes' module" doric)
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom jp-emacs-completion-ui 'vertico
  "Choose minibuffer completion UI between `mct' or `vertico'.
If the value is nil, the default completion user interface is
used.  On Emacs 30, this is close the experience with `mct'.

This user option must be set in the `jp-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'jp-emacs
  :type '(choice :tag "Minibuffer user interface"
                 (const :tag "Default user interface" nil)
                 (const :tag "The `mct' module" mct)
                 (const :tag "The `vertico' module" vertico)))

(defcustom jp-emacs-completion-extras t
  "When non-nil load extras for minibuffer completion.
These include the packages `marginalia', `consult', `corfu',
`orderless', and `embark'."
  :group 'jp-emacs
  :type 'boolean)

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq default-input-method "spanish-prefix") ; also check "greek-postfix"
(setq default-transient-input-method "spanish-prefix")

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region diff-restrict-view))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)

(defvar kiss-quotes
  '("Simplicity is the ultimate sophistication."
    "Everything should be made as simple as possible, but not simpler."
    "The best code is no code at all."
    "If you can't explain it simply, you don't understand it well enough."
    "Deleted code is debugged code."
    "KISS: Keep It Simple, Stupid.")
  "A list of KISS-themed quotes.")

(setq initial-scratch-message
      (let ((chosen-quote (nth (random (length kiss-quotes)) kiss-quotes)))
        (concat
         ;; Part 1: Your original introduction
         (format ";; This is `%s'. Type `%s' to evaluate and print results.\n\n"
                 'lisp-interaction-mode
                 (propertize
                  (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
                  'face 'help-key-binding))
         ;; Part 2: The empty line and the quote
         (format ";; %s\n\n" chosen-quote))))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("jp-lisp" "jp-emacs-modules"))

;;;; Packages

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 4)
        ("melpa" . 3)
	("gnu-elpa-devel" . 2)
        ("nongnu" . 1)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

(defvar jp-emacs-my-packages
  '(agitate
    altcaps
    beframe
    consult-denote
    cursory
    denote
    denote-journal
    denote-markdown
    denote-merge
    denote-org
    denote-silo
    denote-sequence
    dired-preview
    doric-themes
    ef-themes
    fontain
    lin
    logos
    mct
    modus-themes
    notmuch-indicator
    oxford-calendar
    pulsar
    show-font
    spacious-padding
    standard-themes
    substitute
    tmr)
  "List of symbols representing the packages I develop/maintain.")

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-pinned-packages
      (mapcar
       (lambda (package)
         (cons package "gnu-elpa-devel"))
       jp-emacs-my-packages))

;; These are for Emacs 31.
(setq package-review-policy
      (delq nil
            (append
             (mapcar
              (lambda (package)
                (list 'not 'package package))
              jp-emacs-my-packages)
             (mapcar
              (lambda (archive)
                (let ((archive-name (car archive)))
                  (unless (string= archive-name "gnu-elpa-devel")
                    (cons 'archive archive-name))))
              package-archives))))

(setq package-review-diff-command
      (cons diff-command
            '("-u"
              "-x" "'*.elc'"
              "-x" "'*-autoloads.el'"
              "-x" "'*-pkg.el'"
              "-x" "'*.info'"
              "-x" "'*.texi'"
              "-x" "'*.txt'"
              "-x" "'*.md'"
              "-x" "'*.org'")))

(setq custom-safe-themes t)

(defmacro jp-emacs-comment (&rest body)
  "Determine what to do with BODY.

If BODY contains an unquoted plist of the form (:eval t) then
return BODY inside a `progn'.

Otherwise, do nothing with BODY and return nil, with no side
effects."
  (declare (indent defun))
  (let ((eval))
    (dolist (element body)
      (when-let* (((plistp element))
                  (key (car element))
                  ((eq key :eval))
                  (val (cadr element)))
        (setq eval val
              body (delq element body))))
    (when eval `(progn ,@body))))

;; Sample use of `jp-emacs-comment'.  The function
;; `jp-emacs-insert-comment-macro' is never evaluated.
(jp-emacs-comment
  (defun jp-emacs-insert-comment-macro (beg end)
    "Wrap region between BEG and END in `jp-emacs-comment'."
    (interactive "r")
    (if (region-active-p)
        (let ((text (buffer-substring beg end)))
          (delete-region beg end)
          (insert (format "(jp-emacs-comment\n%s)" text))
          (indent-region beg end))
      (user-error "No active region; will not insert `jp-emacs-comment' here"))))

(defmacro jp-emacs-install (package &rest vc-args)
  "Prepare to install PACKAGE.
PACKAGE is an unquoted symbol, referring to the name of the package.  If
VC-ARGS are nil, then install PACKAGE using `package-install'.

If VC-ARGS is non-nil, then check if their `car' is a directory.  If it
is, apply `package-vc-install-from-checkout' on VC-ARGS, else apply
`package-vc-install'.

At all times, do nothing if PACKAGE is already installled."
  (declare (indent 0))
  (unless (symbolp package)
    (error "The package `%s' is not a symbol" package))
  (cond
   ((and package vc-args)
    (let ((fn (if-let* ((first (car vc-args))
                        (_ (and (stringp first) (file-directory-p first))))
                  'package-vc-install-from-checkout
                'package-vc-install)))
      `(unless (package-installed-p ',package)
         (condition-case-unless-debug err
             (apply #',fn ',vc-args)
           (error (message "Failed `%s' with `%S': `%S'" ',fn ',vc-args (cdr err)))))))
   (package
    `(progn
       (unless (package-installed-p ',package)
         (unless package-archive-contents
           (package-refresh-contents))
         (condition-case-unless-debug nil
             (package-install ',package)
           (error (message "Cannot install `%s'; try `M-x package-refresh-contents' first" ',package))))))))

(defmacro jp-emacs-hook (hooks functions &optional remove after)
  "For each HOOKS `add-hook' the FUNCTIONS.
With optional REMOVE as non-nil, then `remove-hook' the FUNCTIONS from
HOOKS.

With optional AFTER as the unquoted symbol of a feature, do so after the
given feature is available."
  (declare (indent 0))
  (cond
   ((symbolp hooks)
    (setq hooks (list hooks)))
   ((not (proper-list-p hooks))
    (error "The hooks are not a list: `%S'" hooks)))
  (cond
   ((symbolp functions)
    (setq functions (list functions)))
   ((not (proper-list-p functions))
    (error "The functions are not a list: `%S'" functions)))
  (let* ((fn (if remove 'remove-hook 'add-hook))
         (body (mapcar
                (lambda (h)
                  (mapcar
                   (lambda (f) `(,fn ',h #',f))
                   functions))
                hooks))
         (hooks nil))
    (dolist (element body)
      (dolist (hook element)
        (push hook hooks)))
    (setq hooks (nreverse hooks))
    (cond
     (after
      `(with-eval-after-load ',after ,@hooks))
     ((length> hooks 1)
      `(progn ,@hooks))
     (t
      (car hooks)))))

(defmacro jp-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let* (((keymapp ,keymap))
                 (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Sample of `jp-emacs-keybind'

;; (jp-emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;; ;; Notice the -map as I am binding keymap here, not a command:
;;   "C-c b" beframe-prefix-map
;;   "C-x k" #'kill-buffer)

(defmacro jp-emacs-autoload (functions file)
  "Declare autoloads for FUNCTIONS for FILE."
  (declare (indent 0))
  (when (symbolp functions)
    (setq functions (list functions)))
  (unless (listp functions)
    (error "The functions must be a list or symbol: %S" functions))
  (unless (stringp file)
    (error "The file must be a string: %S" file))
  (if (length> functions 1)
      `(progn ,@(mapcar (lambda (f) `(autoload #',f ,file)) functions))
    `(autoload #',(car functions) ,file)))

(defmacro jp-emacs-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of (i) string pairs mapping the
abbreviation to its expansion or (ii) a string and symbol pair
making an abbreviation to a function."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
                (if (stringp expansion)
                    `(define-abbrev ,table ,abbrev ,expansion)
                  `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split definitions 2)))
     (error "%s is not an abbrev table" ,table)))

(defmacro jp-emacs-configure (&rest body)
  "Evaluate BODY and catch any errors."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     ((error user-error quit)
      (message "Failed to configure package starting with `%S' because of `%S'" (car ',body) (cdr err)))))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/jp-emacs-pre-custom.el
;;
;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.
(load (locate-user-emacs-file "jp-emacs-pre-custom.el") :no-error :no-message)

(defvar jp-display-graphic-p (display-graphic-p)
  "When non-nil, the display is graphical.")

(defun jp-emacs-gnome-prefers-dark-p ()
  "Return non-nil if GNOME color-scheme is set to dark."
  (string-match-p
   "dark"
   (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")))

 (require 'jp-emacs-theme)
 (require 'jp-emacs-essentials)
 (require 'jp-emacs-ef-themes)
 (require 'jp-emacs-modeline)
 (require 'jp-emacs-completion)
 (require 'jp-emacs-search)
 (require 'jp-emacs-dired)
 (require 'jp-emacs-window)
 (require 'jp-emacs-git)
 (require 'jp-emacs-org)
 (require 'jp-emacs-langs)
 (require 'jp-emacs-spell)
 (require 'jp-emacs-mu4e)
;; (require 'jp-emacs-web)
 (require 'jp-emacs-which-key)
 (require 'jp-emacs-icons)
 (require 'jp-emacs-evil)
 (require 'jp-emacs-general)
 (require 'jp-emacs-0x0)
 (require 'jp-emacs-yasnippets)
 (require 'jp-emacs-treesitter)
 (require 'jp-emacs-eglot)
 (require 'jp-emacs-utils)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/jp-emacs-post-custom.el
;;
;; The purpose of the "post customisations" is to evaluate arbitrary
;; code AFTER loading all my configurations.
(load (locate-user-emacs-file "jp-emacs-post-custom.el") :no-error :no-message)
