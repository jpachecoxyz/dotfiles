;;; custom-modeline.el --- A simple mode-line configuration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; A simple mode-line configuration for Emacs.
;; To enable, put this code in your init file:
;; (require 'custom-modeline)
;; (custom-modeline-mode 1)

;;; Code:

(require 'custom-modeline-core)
(require 'custom-modeline-segments)

(defvar custom-modeline--mode-line
  '((:eval
     (custom-modeline--format
      (car custom-modeline-segments)
      (cadr custom-modeline-segments)))))

;;;###autoload
(define-minor-mode custom-modeline-mode
  "Minor mode to get a simple mode line.

When called interactively, toggle
`custom-modeline-mode'.  With prefix ARG, enable
`custom-modeline--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `custom-modeline-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `custom-modeline-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'custom-modeline
  :global t
  (if custom-modeline-mode
      (progn
        ;; Set the new mode-line-format
        (setq-default mode-line-format '(:eval custom-modeline--mode-line)))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format custom-modeline--default-mode-line))))

(provide 'custom-modeline)
;;; custom-modeline.el ends here
