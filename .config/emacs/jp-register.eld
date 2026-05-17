;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #("(jp-emacs-configure\n  (jp-emacs-install telega)\n  (setq telega-chat-input-markups '(\"markdown2\" \"org\" nil))\n  ;; use vertico for completion\n  (setq telega-completing-read-function 'completing-read)\n  ;; use shift enter to make multi line messages and enter to send it\n  (define-key telega-chat-mode-map (kbd \"S-<return>\") #'newline)\n  ;; disable copy message link when moving over text with evil-mode\n  (define-key telega-msg-button-map (kbd \"l\") nil)\n  ;; avoid showing blank spaces highlighted\n  (add-hook 'telega-chat-mode-hook #'(lambda ()\n                                       (setq-local show-trailing-whitespace nil))))\n\n(defun my/start-telega ()\n  \"Start `telega' inside a new perspective and activate 'telega-mode-line-mode'\"\n  (interactive)\n  (persp-switch \"*telega*\")\n  (telega)\n  (telega-mode-line-mode))\n\n(provide 'jp-emacs-telega)\n"
      0 1
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1 19
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      19 22 (yank-handler (evil-yank-line-handler nil t) fontified t)
      22 23
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      23 39
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      39 46 (yank-handler (evil-yank-line-handler nil t) fontified t)
      46 47
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      47 50 (yank-handler (evil-yank-line-handler nil t) fontified t)
      50 51
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      51 55
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      55 83 (yank-handler (evil-yank-line-handler nil t) fontified t)
      83 84
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      84 95
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      95 96 (yank-handler (evil-yank-line-handler nil t) fontified t)
      96 101
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      101 105
      (yank-handler (evil-yank-line-handler nil t) fontified t) 105
      106
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      106 107
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      107 110
      (yank-handler (evil-yank-line-handler nil t) fontified t) 110
      113
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      113 140
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      140 142
      (yank-handler (evil-yank-line-handler nil t) fontified t) 142
      143
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      143 147
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      147 196
      (yank-handler (evil-yank-line-handler nil t) fontified t) 196
      197
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      197 200
      (yank-handler (evil-yank-line-handler nil t) fontified t) 200
      203
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      203 268
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      268 270
      (yank-handler (evil-yank-line-handler nil t) fontified t) 270
      271
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      271 303
      (yank-handler (evil-yank-line-handler nil t) fontified t) 303
      304
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      304 308
      (yank-handler (evil-yank-line-handler nil t) fontified t) 308
      320
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      320 321
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      321 331
      (yank-handler (evil-yank-line-handler nil t) fontified t) 331
      332
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      332 335
      (yank-handler (evil-yank-line-handler nil t) fontified t) 335
      338
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      338 401
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      401 403
      (yank-handler (evil-yank-line-handler nil t) fontified t) 403
      404
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      404 437
      (yank-handler (evil-yank-line-handler nil t) fontified t) 437
      438
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      438 442
      (yank-handler (evil-yank-line-handler nil t) fontified t) 442
      445
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      445 446
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      446 450
      (yank-handler (evil-yank-line-handler nil t) fontified t) 450
      451
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      451 454
      (yank-handler (evil-yank-line-handler nil t) fontified t) 454
      457
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      457 496
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      496 498
      (yank-handler (evil-yank-line-handler nil t) fontified t) 498
      499
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      499 533
      (yank-handler (evil-yank-line-handler nil t) fontified t) 533
      534
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      534 540
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      540 541
      (yank-handler (evil-yank-line-handler nil t) fontified t) 541
      542
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      542 543
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      543 583
      (yank-handler (evil-yank-line-handler nil t) fontified t) 583
      584
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      584 594
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      594 623
      (yank-handler (evil-yank-line-handler nil t) fontified t) 623
      624
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      624 625
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      625 626
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      626 627
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      627 629
      (yank-handler (evil-yank-line-handler nil t) fontified t) 629
      630
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      630 635
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      635 636
      (yank-handler (evil-yank-line-handler nil t) fontified t) 636
      651
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      651 652
      (yank-handler (evil-yank-line-handler nil t) fontified t) 652
      653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      653 654
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      654 657
      (yank-handler (evil-yank-line-handler nil t) fontified t) 657
      665
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      665 671
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-doc-face)
                    fontified t)
      671 712
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      712 733
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-doc-face)
                    fontified t)
      733 735
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      735 738
      (yank-handler (evil-yank-line-handler nil t) fontified t) 738
      739
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      739 750
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      750 751
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      751 754
      (yank-handler (evil-yank-line-handler nil t) fontified t) 754
      755
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      755 768
      (yank-handler (evil-yank-line-handler nil t) fontified t) 768
      778
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      778 779
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      779 782
      (yank-handler (evil-yank-line-handler nil t) fontified t) 782
      783
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      783 789
      (yank-handler (evil-yank-line-handler nil t) fontified t) 789
      790
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      790 793
      (yank-handler (evil-yank-line-handler nil t) fontified t) 793
      794
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      794 815
      (yank-handler (evil-yank-line-handler nil t) fontified t) 815
      816
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      816 817
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      817 819
      (yank-handler (evil-yank-line-handler nil t) fontified t) 819
      820
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      820 827
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      827 829
      (yank-handler (evil-yank-line-handler nil t) fontified t) 829
      844
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      844 845
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      845 846
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
