;;; custom-modeline-core.el --- The core libraries for custom-modeline -*- lexical-binding: t; -*-

;;; Commentary:

;; The core libraries for custom-modeline.

;;; Code:

(defgroup custom-modeline nil
  "A simple mode line."
  :prefix "custom-modeline-"
  :group 'mode-line)

(defvar custom-modeline--default-mode-line mode-line-format
  "The former value of `mode-line-format'.")

;;
;; Options
;;

(defcustom custom-modeline-segments
  '((custom-modeline-segment-modified
     custom-modeline-segment-buffer-name
     custom-modeline-segment-position)
    (custom-modeline-segment-minor-modes
     custom-modeline-segment-input-method
     custom-modeline-segment-eol
     custom-modeline-segment-encoding
     custom-modeline-segment-vc
	 custom-modeline-segment-misc-info
	 custom-modeline-segment-(point)rocess
	 custom-modeline-segment-major-mode))
  "Simple modeline segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function))
  :package-version '(custom-modeline . "1.2"))

;;
;; Faces
;;

(defface custom-modeline-space
  '((t))
  "Face for space used to alight the right segments in the mode-line.")

(defface custom-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face for less important mode-line elements.")

(defface custom-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface custom-modeline-status-info
  '((t (:inherit (font-lock-string-face))))
  "Face for generic status indicators in the mode-line.")

(defface custom-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface custom-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface custom-modeline-status-error
  '((t (:inherit (error))))
  "Face for error status indicators in the mode-line.")

;;
;; Helpers
;;

(defun custom-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (custom-modeline--format-segments left-segments))
         (right (custom-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit custom-modeline-space))
     right)))

(defun custom-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(provide 'custom-modeline-core)
;;; custom-modeline-core.el ends here
