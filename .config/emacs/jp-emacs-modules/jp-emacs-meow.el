(jp-emacs-configure
  (jp-emacs-install meow)
 ;; emacs keybind to access local leader map
  (setq emacs-local-leader-prefix "C-?")
  ;; meow keybind alias for local leader map
  (setq meow-local-leader-prefix "/")
  (setq meow-local-leader-insert-prefix "C-/")
  ;; keep the expand hints around a while longer
  (setq meow-expand-hint-remove-delay 3.0)
  ;; start git commits in insert mode
  (add-hook 'git-commit-mode-hook 'meow-insert-mode)
  ;; turn it on, baby
  (meow-global-mode 1)

;;(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(setq global-leader-map
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "c") 'meow-keypad-start)
        (define-key keymap (kbd "g") 'meow-keypad-start)
        (define-key keymap (kbd "h") 'meow-keypad-start)
        (define-key keymap (kbd "m") 'meow-keypad-start)
        (define-key keymap (kbd "x") 'meow-keypad-start)
        keymap))

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 ;; global leader
 `("SPC" . ,global-leader-map)
 )

(meow-normal-define-key
 '("1" . meow-1)
 '("2" . meow-2)
 '("3" . meow-3)
 '("4" . meow-4)
 '("5" . meow-5)
 '("6" . meow-6)
 '("7" . meow-7)
 '("8" . meow-8)
 '("9" . meow-9)
 '("0" . meow-0)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 ;; global leader
 `("SPC" . ,global-leader-map)
 ;; local leader
 `(,meow-local-leader-prefix . ,emacs-local-leader-prefix)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-delete)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("q" . keyboard-quit)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("<escape>" . keyboard-quit))

;; local leader in insert mode
(define-key meow-insert-state-keymap
  (kbd meow-local-leader-insert-prefix)
  (meow--parse-def emacs-local-leader-prefix))

(defmacro def-meow-digit-action (func digit)
  "Create function FUNC that when called will call `meow-expand-DIGIT' when
  expanding, and `meow-digit-argument' otherwise."
  (let ((meow-expand-digit (intern (format "meow-expand-%d" digit))))
    `(defun ,func ()
       (interactive)
       (if (meow-expanding-p)
           (,meow-expand-digit)
         (meow-digit-argument)))))

(defun meow-expanding-p ()
  "Return non-NIL when `meow' is either expanding or selecting text."
  (meow--selection-type))

(def-meow-digit-action meow-1 1)
(def-meow-digit-action meow-2 2)
(def-meow-digit-action meow-3 3)
(def-meow-digit-action meow-4 4)
(def-meow-digit-action meow-5 5)
(def-meow-digit-action meow-6 6)
(def-meow-digit-action meow-7 7)
(def-meow-digit-action meow-8 8)
(def-meow-digit-action meow-9 9)
(def-meow-digit-action meow-0 0)

;; don't need the macro anymore
(fmakunbound 'def-meow-digit-action))

(provide 'jp-emacs-meow)
