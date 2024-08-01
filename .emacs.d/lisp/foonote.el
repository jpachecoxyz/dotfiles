;;; Code:
;;; foonote.el --- Emacs package for managing org notes -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides functionality for creating and managing org notes.

;;; Code:

(defvar foonote-directories '("~/notes")
  "List of folders where org notes will be stored.")

(defvar foonote-default-metadata '()
  "Default metadata for org files.")

(defun foonote-add-directories (directories)
  "Add DIRECTORIES to `foonote-directories`, creating them if they do not exist."
  (dolist (dir directories)
    (if (file-directory-p dir)
        (progn
          (add-to-list 'foonote-directories (expand-file-name dir))
          (message "Directory added: %s" dir))
      (when (y-or-n-p (format "Directory %s does not exist. Create it? " dir))
        (make-directory dir t)
        (add-to-list 'foonote-directories (expand-file-name dir))
        (message "Directory created and added: %s" dir)))))

(declare-function org-id-add-location "org")
(declare-function org-with-point-at "org")
(declare-function org-entry-get "org")
(declare-function org-id-new "org")
(declare-function org-entry-put "org")

;; Copied from this article (with minor tweaks from my side):
;; https://writequit.org/articles/emacs-org-mode-generate-ids.html.
(defun foonote/org--id-get (&optional pom create prefix)
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
(defun foonote/org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (foonote/org--id-get (point) t))))

;;;###autoload
(defun foonote/org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (foonote/org--id-get (point) t))

(defun foonote-add-link ()
  "Insert a link to another note and store it for org-store-link."
  (interactive)
  (evil-insert-state)
  (let ((notes '())
        (note-names '())
        (link-description ""))
    ;; Collect all org files from specified directories
    (dolist (dir foonote-directories)
      (setq notes (append notes (directory-files-recursively dir "\\.org$"))))
    ;; Extract file names from file paths
    (setq note-names (mapcar #'file-name-nondirectory notes))
    ;; Prompt user to select a note
    (if note-names
        (progn
          (let* ((selected-note (completing-read "Select note: " note-names))
                 (selected-file (nth (cl-position selected-note note-names :test #'equal) notes)))
			(setq link-description (read-string "Enter link description: "))
            (insert (format "[[%s][%s]]" selected-file link-description))
            (org-store-link-props :type "custom-id-link" :link selected-file)
            (message "Link to note '%s' inserted and stored for org-store-link." selected-note)))
      (message "No notes found in specified directories."))))

(defun foonote-create ()
  "Create an org note in a specified folder."
  (interactive)
  (let* ((folder (completing-read "Choose folder: " foonote-directories nil t)) ; Prompt to choose a folder
         (note-files (directory-files folder nil "\\.org$")) ; List of existing note files in the folder
         (note-name (completing-read "Enter note name (without extension): " note-files nil nil nil 'foonote-note-name-history)) ; Prompt for note name with autocompletion
         (note-name (if (string-suffix-p ".org" note-name) note-name (concat note-name ".org"))) ; Ensure note name ends with ".org"
		 (tags (unless (file-exists-p (concat folder "/" note-name))
				 (read-string "Enter tags (comma-separated): "))) ; Prompt for tags only if file doesn't exist
		 (note-file (concat folder "/" note-name))) ; Concatenate folder path and note name
	(find-file note-file) ; Open the note file
	(unless (file-exists-p note-file)
	  (insert "#+TITLE: " (file-name-sans-extension note-name) "\n")
	  (dolist (metadata foonote-default-metadata)
		(insert (format "%s\n" metadata)))
	  (when (not (string-empty-p tags))
		(insert (format "#+TAGS: %s\n" (replace-regexp-in-string "," ":" tags)))))
	(foonote/org-id-headline)
	(goto-char (point-max))))
;; (insert "\n"))) ; Move cursor to the end of the buffer

(defvar foonote-note-name-history nil
  "History list for note names in create-org-note function.")

(provide 'foonote)

;;;foonote.el ends here
