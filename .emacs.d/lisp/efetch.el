;;; efetch.el --- A minimal fetching tool written in emacs-lisp -*- lexical-binding: t -*-

;; Author: Javier Pacheco
;; Maintainer: Javier Pacheco
;; Version: 0.1
;; Homepage: https://github.com/engjpacheco/efetch
;; Keywords: efetch



;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:

;; This fetching tools open a buffer in the left side of the main frame
;; and provide some information about emacs such packages, font, version,etc.

;; Emacs version:
;; Host:
;; User:
;; Packages:
;; Active theme:
;; Default font:



;; Example of usage:

;; (require 'efetch)
;; (global-set-key (kbd "<f5>") 'efetch)



;;; Code:

(defun efetch ()
  "Display custom information in a left buffer."
  (interactive)
  (let* ((header-text (propertize "efetch - emacs fetching tool" 'face '(:weight bold :foreground "#41728e")))
         (emacs-version-text (propertize (format "Emacs version: %s" emacs-version) 'face '(:foreground "#b5bd68")))
         (OS (propertize (format "Host: %s" (shell-command-to-string "uname -o")) 'face '(:foreground "#5a5b5a")))
         (username (propertize (format "User: %s" (getenv "USER")) 'face '(:foreground "#8abeb7")))
         (packages-installed-text (propertize (format "Packages installed: %d" (length package-activated-list)) 'face '(:foreground "#cc6666")))
         (theme-text (propertize (format "Active theme: %s" (or (car custom-enabled-themes) "None")) 'face '(:foreground "#b294bb")))
         (font-text (propertize (format "Default font: %s" (face-attribute 'default :family)) 'face '(:foreground "#de935f")))
         (fetch-text (format "%s\n%s\n%s%s\n%s\n%s\n%s\n"
                             header-text
                             emacs-version-text
                             OS
                             username
                             packages-installed-text
                             theme-text
                             font-text)))
    (with-current-buffer (get-buffer-create "*efetch-popup*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert fetch-text)
      (setq buffer-read-only t)
      (local-set-key "q" #'kill-buffer-and-window)
      (display-buffer-in-side-window (current-buffer) '((side . left) (window-width . 40)))
      (sit-for 7) ; Adjust as needed
      (kill-buffer-and-window))))

(provide 'efetch)

;;; efetch.el ends here
