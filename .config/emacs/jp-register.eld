;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #("  (defun org-todo-if-needed (state)\n    \"Change header state to STATE unless the current item is in STATE already.\"\n    (unless (string-equal (org-get-todo-state) state)\n      (org-todo state)))\n\n  (defun ct/org-summary-todo-cookie (n-done n-not-done)\n    \"Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise\"\n    (let (org-log-done org-log-states)   ; turn off logging\n      (org-todo-if-needed (cond ((= n-done 0)\n                                 \"TODO\")\n                                ((= n-not-done 0)\n                                 \"DONE\")\n                                (t\n                                 \"DOING\")))))\n  (add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)\n\n  (defun ct/org-summary-checkbox-cookie ()\n    \"Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise\"\n    (let (beg end)\n      (unless (not (org-get-todo-state))\n        (save-excursion\n          (org-back-to-heading t)\n          (setq beg (point))\n          (end-of-line)\n          (setq end (point))\n          (goto-char beg)\n          ;; Regex group 1: %-based cookie\n          ;; Regex group 2 and 3: x/y cookie\n          (if (re-search-forward \"\\\\[\\\\([0-9]*%\\\\)\\\\]\\\\|\\\\[\\\\([0-9]*\\\\)/\\\\([0-9]*\\\\)\\\\]\"\n                                 end t)\n              (if (match-end 1)\n                  ;; [xx%] cookie support\n                  (cond ((equal (match-string 1) \"100%\")\n                         (org-todo-if-needed \"DONE\"))\n                        ((equal (match-string 1) \"0%\")\n                         (org-todo-if-needed \"TODO\"))\n                        (t\n                         (org-todo-if-needed \"DOING\")))\n                ;; [x/y] cookie support\n                (if (> (match-end 2) (match-beginning 2)) ; = if not empty\n                    (cond ((equal (match-string 2) (match-string 3))\n                           (org-todo-if-needed \"DONE\"))\n                          ((or (equal (string-trim (match-string 2)) \"\")\n                               (equal (match-string 2) \"0\"))\n                           (org-todo-if-needed \"TODO\"))\n                          (t\n                           (org-todo-if-needed \"DOING\")))\n                  (org-todo-if-needed \"DOING\"))))))))\n  (add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)\n"
      0 2 (yank-handler (evil-yank-line-handler nil t) fontified t) 2
      3
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      3 8
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      8 9
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      9 27
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      27 28
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      28 29
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      29 34
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      34 35
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      35 36
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      36 40
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      40 115
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t font-lock-fontified
                    t src-block t)
      115 120
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      120 121
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      121 127
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      127 128
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      128 129
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      129 142
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      142 143
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      143 161
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      161 162
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      162 168
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      168 169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      169 170
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      170 176
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      176 177
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      177 191
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      191 192
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      192 193
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      193 194
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      194 195
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      195 196
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      196 198
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      198 199
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      199 204
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      204 205
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      205 208
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      208 231
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      231 232
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      232 233
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      233 250
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      250 251
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      251 256
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      256 366
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t font-lock-fontified
                    t src-block t)
      366 371
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      371 372
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      372 375
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      375 376
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      376 377
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      377 404
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      404 405
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      405 408
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      408 410
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      410 427
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      427 433
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      433 434
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      434 453
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      453 454
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      454 458
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      458 459
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      459 460
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      460 461
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      461 464
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      464 471
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      471 472
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      472 506
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      506 512
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      512 513
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      513 546
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      546 547
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      547 548
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      548 562
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      562 563
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      563 597
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      597 603
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      603 604
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      604 637
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      637 638
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      638 673
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      673 680
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      680 681
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      681 682
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      682 683
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      683 684
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      684 685
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      685 688
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      688 689
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      689 758
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      758 759
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      759 760
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      760 761
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      761 763
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      763 764
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      764 769
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      769 770
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      770 800
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      800 801
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      801 802
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      802 803
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      803 808
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      808 922
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t font-lock-fontified
                    t src-block t)
      922 927
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      927 928
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      928 931
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      931 932
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      932 933
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      933 940
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      940 941
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      941 942
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      942 948
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      948 949
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      949 955
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      955 956
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      956 957
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      957 961
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      961 962
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      962 980
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      980 981
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      981 982
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      982 991
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      991 992
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      992 1006
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1006 1007
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1007 1017
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1017 1018
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1018 1039
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1039 1040
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1040 1041
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1041 1051
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1051 1052
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1052 1056
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1056 1061
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1061 1062
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1062 1067
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1067 1068
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1068 1069
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1069 1070
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1070 1080
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1080 1081
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1081 1092
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1092 1093
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1093 1094
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1094 1104
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1104 1105
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1105 1109
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1109 1114
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1114 1115
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1115 1120
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1120 1121
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1121 1122
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1122 1123
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1123 1133
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1133 1134
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1134 1147
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1147 1148
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1148 1149
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1149 1159
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1159 1162
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      1162 1192
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      1192 1202
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1202 1205
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      1205 1237
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      1237 1247
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1247 1248
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1248 1250
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1250 1251
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1251 1252
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1252 1270
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1270 1274
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1274 1276
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1276 1277
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1277 1284
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1284 1286
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1286 1287
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1287 1290
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1290 1292
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1292 1293
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1293 1296
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1296 1298
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1298 1299
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1299 1305
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1305 1307
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1307 1308
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1308 1309
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1309 1311
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1311 1312
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1312 1318
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1318 1320
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1320 1321
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t font-lock-fontified t src-block t)
      1321 1325
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1325 1359
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1359 1362
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1362 1364
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1364 1365
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1365 1366
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1366 1380
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1380 1381
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1381 1383
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1383 1384
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1384 1385
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      1385 1396
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1396 1397
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      1397 1398
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1398 1416
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1416 1419
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      1419 1440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      1440 1458
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1458 1459
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      1459 1463
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1463 1464
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1464 1465
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1465 1466
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1466 1472
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1472 1473
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1473 1487
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1487 1488
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1488 1489
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1489 1495
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1495 1496
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1496 1497
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1497 1500
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1500 1522
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1522 1523
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1523 1535
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1535 1536
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1536 1542
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1542 1548
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1548 1549
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1549 1550
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1550 1575
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1575 1576
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1576 1577
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1577 1583
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1583 1584
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1584 1598
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1598 1599
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1599 1600
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1600 1604
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1604 1605
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1605 1631
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1631 1632
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1632 1651
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1651 1657
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1657 1658
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1658 1659
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1659 1684
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1684 1685
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1685 1687
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1687 1708
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1708 1712
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1712 1713
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1713 1732
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1732 1739
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1739 1740
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1740 1741
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1741 1742
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      1742 1759
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1759 1762
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      1762 1783
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      1783 1799
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1799 1800
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      1800 1802
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1802 1803
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1803 1804
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1804 1806
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1806 1807
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1807 1818
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1818 1819
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1819 1820
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1820 1821
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1821 1838
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1838 1839
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1839 1840
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1840 1841
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1841 1843
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t
                    font-lock-fontified t src-block t)
      1843 1858
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t
                    font-lock-fontified t src-block t)
      1858 1878
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1878 1879
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      1879 1883
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      1883 1884
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1884 1885
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1885 1886
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1886 1892
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1892 1893
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      1893 1907
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1907 1908
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      1908 1909
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1909 1910
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      1910 1924
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1924 1925
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      1925 1926
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1926 1927
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1927 1954
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1954 1955
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1955 1964
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1964 1974
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1974 1980
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      1980 1981
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      1981 1982
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      1982 1983
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      1983 2009
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2009 2010
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      2010 2011
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2011 2013
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      2013 2014
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2014 2015
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      2015 2021
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2021 2022
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2022 2034
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2034 2035
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      2035 2049
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2049 2050
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      2050 2051
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2051 2052
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2052 2054
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2054 2055
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      2055 2056
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2056 2087
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2087 2088
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      2088 2094
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2094 2095
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2095 2109
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2109 2110
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2110 2111
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2111 2114
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2114 2115
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      2115 2116
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2116 2117
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2117 2144
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2144 2145
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2145 2164
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2164 2170
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2170 2171
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2171 2172
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      2172 2173
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2173 2199
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2199 2200
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      2200 2202
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2202 2229
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2229 2230
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2230 2249
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2249 2256
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2256 2257
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2257 2258
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      2258 2259
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2259 2260
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2260 2278
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2278 2279
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2279 2298
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2298 2305
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2305 2306
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2306 2307
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t
                    font-lock-fontified t src-block t)
      2307 2308
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t
                    font-lock-fontified t src-block t)
      2308 2309
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t
                    font-lock-fontified t src-block t)
      2309 2310
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t
                    font-lock-fontified t src-block t)
      2310 2311
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t
                    font-lock-fontified t src-block t)
      2311 2312
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t
                    font-lock-fontified t src-block t)
      2312 2313
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2313 2316
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2316 2317
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2317 2388
      (yank-handler (evil-yank-line-handler nil t) fontified t
                    font-lock-fontified t src-block t)
      2388 2389
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t
                    font-lock-fontified t src-block t)
      2389 2390
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
