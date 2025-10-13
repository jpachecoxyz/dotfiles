;;; utilities.el --- Some custom usefull utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;  Some utilities that I Have been written to my personal use.

;;; Code:

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
  "Toggle emphasis markers and display a message."
  (interactive "p")
  (let ((markers org-hide-emphasis-markers)
        (msg ""))
    (when markers
      (setq-local org-hide-emphasis-markers nil)
      (setq msg "Emphasis markers are now visible."))
    (unless markers
      (setq-local org-hide-emphasis-markers t)
      (setq msg "Emphasis markers are now hidden."))
    (message "%s" msg)
    (when arg
      (font-lock-fontify-buffer))))

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

(defvar report-file "~/Desktop/report.org"
  "Path to the Org-mode file to store the failure/solution report.")

(defun report-failure-solution ()
  "Prompt for machine number, failure, solution, and repair time. Append to an Org-mode report file."
  (interactive)
  (let ((continue-loop t))
    (catch 'exit
      (while continue-loop
        (let* ((machine-number (read-string "Enter machine number (or 'exit' to finish): "))
               (lowercase-machine (downcase machine-number)))
          (when (equal lowercase-machine "exit")
            (message "Exiting failure/solution report.")
            (setq continue-loop nil)
            (throw 'exit nil))

          (let* ((failure (read-string "Enter failure: "))
                 (solution (read-string "Enter solution: "))
                 (repair-time (read-string "Enter repair time (e.g., 2 hours): "))
                 (marker (concat "Machine " machine-number)))

            (with-temp-buffer
              (insert-file-contents report-file)
              (goto-char (point-min))

              (if (re-search-forward marker nil t)
                  (progn
                    (goto-char (line-end-position))
                    (insert "\n*** Failure: " failure "\n")
                    (insert "    - Solution: " solution "\n")
                    (insert "    - Repair Time: " repair-time "\n"))
                (goto-char (point-max))
                (insert "\n* " marker "\n")
                (insert "** Failure: " failure "\n")
                (insert "   - Solution: " solution "\n")
                (insert "   - Repair Time: " repair-time "\n"))

              (write-region (point-min) (point-max) report-file nil 'append))))))))

(global-set-key (kbd "C-c <f12>") 'report-failure-solution)

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

(defun new-scratch-pad ()
  "Create a new org-mode buffer for random stuff."
  (interactive)
  (progn
    (let ((buffer (generate-new-buffer "Org-scratch-buffer")))
      (switch-to-buffer buffer)
      (setq buffer-offer-save t)
      (org-mode))))

;; Toggle *scratch* buffer.
(defun toggle-scratch-buffer ()
  "Toggle the *scratch* buffer"
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (bury-buffer)
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defun toggle-org-buffer ()
  "Toggle the Org-scratch-buffer buffer"
  (interactive)
  (if (equal (buffer-name (current-buffer)) "Org-scratch-buffer")
      (if (one-window-p t)
          (switch-to-buffer (other-buffer))
        (delete-window))
    (if (get-buffer "Org-scratch-buffer")
        (if (get-buffer-window "Org-scratch-buffer")
            (progn
              (bury-buffer "Org-scratch-buffer")
              (delete-window (get-buffer-window "Org-scratch-buffer")))
          (switch-to-buffer "Org-scratch-buffer"))
      (new-scratch-pad))))

;; Toggle *eshell* buffer.
(defun toggle-eshell-buffer ()
  "Toggle the visibility of the *eshell* buffer.
If already in the buffer, bury it. Otherwise, switch to it or launch Eshell."
  (interactive)
  (let ((buf (get-buffer "*eshell*")))
    (if (string= (buffer-name) "*eshell*")
        (bury-buffer)
      (if buf
          (switch-to-buffer buf)
        (eshell)))))

(defun my-show-doc-or-describe-symbol ()
  "Show LSP UI doc if LSP is active, otherwise describe symbol at point."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-doc-glance)
    (describe-symbol-at-point)))

(defun duplicate-line ()
  (interactive)
  (let ((line-text (thing-at-point 'line t)))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert line-text)))
  (forward-line 1))

(defun move-line-up ()
  (interactive)
  (when (not (= (line-number-at-pos) 1))
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (when (not (= (line-number-at-pos) (point-max)))
    (transpose-lines 1))
  (forward-line -1))

;; Define a custom face for the highlight
(defface my-highlight-face
  '((t (:foreground "gray"))) ; Change "red" to your desired color
  "Face for highlighting !!word!! patterns.")

;; Function to replace matched text with asterisks
(defun replace-with-asterisks (limit)
  "Replace !!word!! with asterisks up to LIMIT."
  (while (re-search-forward "!!\\(.*?\\)!!" limit t)
    (let* ((match (match-string 1))
           (start (match-beginning 0))
           (end (match-end 0))
           (asterisks (make-string (length match) ?*)))
      (add-text-properties start end `(display ,asterisks face my-highlight-face)))))

;; Add custom keyword for font-lock in org-mode
(defun my/org-mode-custom-font-lock ()
  "Add custom font-lock keywords for org-mode."
  (font-lock-add-keywords nil
                          '((replace-with-asterisks))))

;; Hook the custom font-lock configuration into org-mode
(add-hook 'org-mode-hook 'my/org-mode-custom-font-lock)

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

(defun custom-jp-themes (&optional theme-dir)
  "Return a list of custom themes from a specified directory.
Search the directory for files named FOO-theme.el, and return a list of FOO symbols,
excluding the 'default' theme and any internal themes.

If THEME-DIR is nil, it defaults to `~/.emacs.d/lisp/jp-themes/'."
  (let ((suffix "-theme\\.el\\'")
        (directory (or theme-dir "~/.emacs.d/lisp/jp-themes/"))
        themes)
    ;; Ensure the directory exists
    (when (file-directory-p directory)
      ;; Iterate over all theme files in the directory
      (dolist (file (directory-files directory nil suffix))
        (let ((theme (intern (substring file 0 (string-match-p suffix file)))))
          ;; Add to the list if it's valid, and exclude Emacs built-in "default" theme
          (and (not (eq theme 'default))  ;; Ensure "default" is excluded
             (not (memq theme themes))  ;; Avoid duplicates
             (push theme themes)))))
    (nreverse themes)))

(defcustom fz-themes nil
  "List of themes (symbols or regexps) to be presented for selection.
nil shows all `custom-available-themes'."
  :type '(repeat (choice symbol regexp)))

(defun fz-theme (theme)
  "Disable current themes and enable THEME from `fz-themes`.

The command supports previewing the currently selected theme."
  (interactive
   (list
    (let* ((regexp (consult--regexp-filter
                    (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                            fz-themes)))
           (avail-themes (seq-filter
                          (lambda (x) (string-match-p regexp (symbol-name x)))
                          (custom-jp-themes)))  ;; Only use themes from custom-jp-themes
           (saved-theme (car custom-enabled-themes)))
      (consult--read
       (mapcar #'symbol-name avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (selected &rest _)
                 (setq selected (and selected (intern-soft selected)))
                 (or (and selected (car (memq selected avail-themes)))
                    saved-theme))
       :state (lambda (action theme)
                (pcase action
                  ('return (fz-theme (or theme saved-theme)))
                  ((and 'preview (guard theme)) (fz-theme theme))))
       :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

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
  "Run the publish blog script within Emacs and display a success message in the minibuffer."
  (interactive)
  (let ((commit-msg (read-string "Enter commit message: ")))
    ;; Just run the publish script
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
  (let ((choice (completing-read "Choose an option: " '("config" "scripts" "notes" "pdf's" "src" "books" "docs"))))
    (cond
     ((string= choice "config")
      (fzf-find-file "~/.config/"))
     ((string= choice "scripts")
      (fzf-find-file "~/.local/bin/"))
     ((string= choice "src")
      (fzf-find-file "~/.local/src/"))
     ((string= choice "notes")
      (consult-find "~/Documents/Emacs/notes/"))
     ((string= choice "pdf's")
      (consult-find "~/Documents/Emacs/notes/pdf/"))
     ((string= choice "books")
      (consult-find "~/Documents/Emacs/books/"))
     ((string= choice "docs")
      (consult-find "~/Documents/Emacs/typst/"))
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


;; (prog1 'my/transient-goto-file-buffer
;;   ;; List
;;   (setq my/goto-file-buffer-alist
;;         '(("s" "*scratch*"    (switch-to-buffer "*scratch*"))
;;           ("h" "home.nix"       (find-file "~/.dotfiles/nix/home.nix"))
;;           ("c" "configuration.nix"       (find-file "~/.dotfiles/nix/configuration.nix"))
;;           ("u" "utilities.org"       (find-file "~/.emacs.d/lisp/utilities.org"))
;;           ("j" "Journal"      (find-file (expand-file-name "agenda/journal.org" org-directory)))
;;           ("n" "Notes"        (find-file (expand-file-name "agenda/notes.org" org-directory)))))
;;   ;; Command
;;   (defun my/goto-file-buffer ()
;;     "Jump to a file or buffer based on the key used to invoke this command."
;;     (interactive)
;;     (let* ((key (this-command-keys))
;;            (choice (assoc key my/goto-file-buffer-alist))
;;            (form (caddr choice)))
;;       (eval form)))
;;   ;; Transient
;;   (eval
;;    `(transient-define-prefix my/transient-goto-file-buffer ()
;;       [,(apply 'vector
;;                "Goto Files & Buffers"
;;                (mapcar (lambda (x) (list (car x) (cadr x) 'my/goto-file-buffer))
;;                        my/goto-file-buffer-alist))])))

(provide 'utilities)
;;; utilities.el ends here
