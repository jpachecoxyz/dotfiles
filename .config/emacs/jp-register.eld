;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((105
  . [101 120 105 116 return escape escape 105 101 120 105 116 return
         101 120 105 116 return escape])
 (97) (113)
 (48
  . #("(defun jp-window-shell-or-term-p (buffer &rest _)\n  \"Check if BUFFER is a shell or terminal.\nThis is a predicate function for `buffer-match-p', intended for\nuse in `display-buffer-alist'.\"\n  (when (string-match-p \"\\\\*.*\\\\(e?shell\\\\|v?term\\\\).*\" (buffer-name (get-buffer buffer)))\n    (with-current-buffer buffer\n      ;; REVIEW 2022-07-14: Is this robust?\n      (and (not (derived-mode-p 'message-mode 'text-mode))\n           (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))\n"
      0 1
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1 6
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      6 7 (yank-handler (evil-yank-line-handler nil t) fontified t) 7
      32
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      32 33 (yank-handler (evil-yank-line-handler nil t) fontified t)
      33 34
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      34 41 (yank-handler (evil-yank-line-handler nil t) fontified t)
      41 46
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      46 48 (yank-handler (evil-yank-line-handler nil t) fontified t)
      48 49
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      49 52 (yank-handler (evil-yank-line-handler nil t) fontified t)
      52 127
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      127 141
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-doc-face)
                    fontified t)
      141 165
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      165 185
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-doc-face)
                    fontified t)
      185 188
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      188 191
      (yank-handler (evil-yank-line-handler nil t) fontified t) 191
      192
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      192 196
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      196 197
      (yank-handler (evil-yank-line-handler nil t) fontified t) 197
      198
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      198 213
      (yank-handler (evil-yank-line-handler nil t) fontified t) 213
      219
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      219 221
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      221 222
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      222 229
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      229 231
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      231 232
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      232 238
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      238 240
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      240 241
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      241 244
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      244 245
      (yank-handler (evil-yank-line-handler nil t) fontified t) 245
      246
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      246 258
      (yank-handler (evil-yank-line-handler nil t) fontified t) 258
      259
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      259 276
      (yank-handler (evil-yank-line-handler nil t) fontified t) 276
      277
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      277 278
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      278 279
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      279 284
      (yank-handler (evil-yank-line-handler nil t) fontified t) 284
      285
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      285 304
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      304 318
      (yank-handler (evil-yank-line-handler nil t) fontified t) 318
      321
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      321 356
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      356 362
      (yank-handler (evil-yank-line-handler nil t) fontified t) 362
      363
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      363 366
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      366 367
      (yank-handler (evil-yank-line-handler nil t) fontified t) 367
      368
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      368 372
      (yank-handler (evil-yank-line-handler nil t) fontified t) 372
      373
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      373 412
      (yank-handler (evil-yank-line-handler nil t) fontified t) 412
      413
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      413 414
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      414 426
      (yank-handler (evil-yank-line-handler nil t) fontified t) 426
      427
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      427 497
      (yank-handler (evil-yank-line-handler nil t) fontified t) 497
      498
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      498 499
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      499 500
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      500 501
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      501 502
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      502 503
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
