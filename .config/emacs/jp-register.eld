;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((97) (113 . [escape escape])
 (48
  . #(";;; Git gutter (simple experimental)\n(jp-emacs-configure\n\n  (require 'cl-lib)\n\n  (defvar emacs-solo-enable-buffer-gutter t)\n\n  (defun jp/git-gutter-goto-next-hunk ()\n    \"Jump cursor to the closest next hunk.\"\n    (interactive)\n    (let* ((current-line (line-number-at-pos))\n           (line-numbers (mapcar #'car git-gutter-diff-info))\n           (sorted-line-numbers (sort line-numbers #'<))\n           (next-line-number\n            (if (not (member current-line sorted-line-numbers))\n                (cl-find-if (lambda (line) (> line current-line)) sorted-line-numbers)\n              (let ((last-line nil))\n                (cl-loop for line in sorted-line-numbers\n                         when (and (> line current-line)\n                                   (or (not last-line)\n                                       (/= line (1+ last-line))))\n                         return line\n                         do (setq last-line line))))))\n\n      (when next-line-number\n        (goto-char (point-min))\n        (forward-line (1- next-line-number)))))\n\n  (defun jp/git-gutter-goto-previous-hunk ()\n    \"Jump cursor to the closest previous hunk.\"\n    (interactive)\n    (let* ((current-line (line-number-at-pos))\n           (line-numbers (mapcar #'car git-gutter-diff-info))\n           (sorted-line-numbers (sort line-numbers #'<))\n           (previous-line-number\n            (if (not (member current-line sorted-line-numbers))\n                (cl-find-if (lambda (line) (< line current-line))\n                            (reverse sorted-line-numbers))\n              (let ((previous-line nil))\n                (dolist (line sorted-line-numbers)\n                  (when (and (< line current-line)\n                             (not (member (1- line) line-numbers)))\n                    (setq previous-line line)))\n                previous-line))))\n\n      (when previous-line-number\n        (goto-char (point-min))\n        (forward-line (1- previous-line-number)))))\n\n  (defun jp/git-gutter-process-git-diff ()\n    \"Process git diff and return changed lines.\"\n    (setq-local result '())\n    (let* ((file-path (buffer-file-name))\n           (grep-command \"rg -Po\")\n           (output\n            (shell-command-to-string\n             (format\n              \"git diff --unified=0 %s | %s '^@@ -[0-9]+(,[0-9]+)? \\\\+\\\\K[0-9]+(,[0-9]+)?(?= @@)'\"\n              file-path\n              grep-command))))\n\n      (setq-local lines (split-string output \"\\n\"))\n\n      (dolist (line lines)\n        (cond\n         ((string-match \"\\\\(^[0-9]+\\\\),\\\\([0-9]+\\\\)\\\\(?:,0\\\\)?$\" line)\n          (let ((num (string-to-number (match-string 1 line)))\n                (count (string-to-number (match-string 2 line))))\n            (if (= count 0)\n                (add-to-list 'result (cons (+ 1 num) \"deleted\"))\n              (dotimes (i count)\n                (add-to-list 'result (cons (+ num i) \"changed\"))))))\n\n         ((string-match \"\\\\(^[0-9]+\\\\)$\" line)\n          (add-to-list 'result (cons (string-to-number line) \"added\")))))\n\n      (setq-local git-gutter-diff-info result)\n      result))\n\n  (defun jp/git-gutter-add-mark (&rest _)\n    \"Display git gutter marks.\"\n    (set-window-margins (selected-window) 2 0)\n\n    (remove-overlays (point-min) (point-max)\n                     'jp--git-gutter-overlay t)\n\n    (let ((lines-status (or (jp/git-gutter-process-git-diff) '())))\n      (save-excursion\n        (dolist (line-status lines-status)\n          (let ((line-num (car line-status))\n                (status (cdr line-status)))\n            (goto-char (point-min))\n            (forward-line (1- line-num))\n\n            (let ((overlay (make-overlay (point-at-bol) (point-at-bol))))\n              (overlay-put overlay 'jp--git-gutter-overlay t)\n              (overlay-put overlay 'before-string\n                           (propertize \" \"\n                                       'display\n                                       `((margin left-margin)\n                                         ,(propertize\n                                           (pcase status\n                                             (\"added\" \"+\")\n                                             (\"changed\" \"~\")\n                                             (\"deleted\" \"_\"))\n                                           'face '(:foreground \"gray\")))))))))))\n\n  (defun jp/git-gutter-on ()\n    (interactive)\n    (jp/git-gutter-add-mark)\n    (add-hook 'find-file-hook #'jp/git-gutter-add-mark)\n    (add-hook 'after-save-hook #'jp/git-gutter-add-mark))\n\n  (defun jp/git-gutter-off ()\n    (interactive)\n    (remove-overlays (point-min) (point-max)\n                     'jp--git-gutter-overlay t)\n    (remove-hook 'find-file-hook #'jp/git-gutter-add-mark)\n    (remove-hook 'after-save-hook #'jp/git-gutter-add-mark))\n\n  ;; Keybindings\n  (keymap-global-set \"M-9\" #'jp/git-gutter-goto-previous-hunk)\n  (keymap-global-set \"M-0\" #'jp/git-gutter-goto-next-hunk)\n\n  (keymap-global-set \"C-c g p\" #'jp/git-gutter-goto-previous-hunk)\n  (keymap-global-set \"C-c g n\" #'jp/git-gutter-goto-next-hunk)\n  (keymap-global-set \"C-c g g\" #'jp/git-gutter-on)\n  (keymap-global-set \"C-c g r\" #'jp/git-gutter-off)\n\n  (when emacs-solo-enable-buffer-gutter\n    (add-hook 'after-init-hook #'jp/git-gutter-on)))\n"
      0 4
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      4 37
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      37 38
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      38 56
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      56 57 (yank-handler (evil-yank-line-handler nil t) fontified t)
      57 58 (yank-handler (evil-yank-line-handler nil t) fontified t)
      58 60 (yank-handler (evil-yank-line-handler nil t) fontified t)
      60 61
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      61 68
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      68 70 (yank-handler (evil-yank-line-handler nil t) fontified t)
      70 76
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      76 77
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      77 78 (yank-handler (evil-yank-line-handler nil t) fontified t)
      78 79 (yank-handler (evil-yank-line-handler nil t) fontified t)
      79 81 (yank-handler (evil-yank-line-handler nil t) fontified t)
      81 82
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      82 88
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      88 89 (yank-handler (evil-yank-line-handler nil t) fontified t)
      89 120
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      120 122
      (yank-handler (evil-yank-line-handler nil t) fontified t) 122
      123
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      123 124
      (yank-handler (evil-yank-line-handler nil t) fontified t) 124
      125 (yank-handler (evil-yank-line-handler nil t) fontified t)
      125 127
      (yank-handler (evil-yank-line-handler nil t) fontified t) 127
      128
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      128 133
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      133 134
      (yank-handler (evil-yank-line-handler nil t) fontified t) 134
      162
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      162 163
      (yank-handler (evil-yank-line-handler nil t) fontified t) 163
      164
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      164 165
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      165 166
      (yank-handler (evil-yank-line-handler nil t) fontified t) 166
      170 (yank-handler (evil-yank-line-handler nil t) fontified t)
      170 209
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      209 210
      (yank-handler (evil-yank-line-handler nil t) fontified t) 210
      214 (yank-handler (evil-yank-line-handler nil t) fontified t)
      214 215
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      215 226
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      226 227
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      227 232
      (yank-handler (evil-yank-line-handler nil t) fontified t) 232
      233
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      233 237
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      237 238
      (yank-handler (evil-yank-line-handler nil t) fontified t) 238
      239
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      239 240
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      240 253
      (yank-handler (evil-yank-line-handler nil t) fontified t) 253
      254
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      254 272
      (yank-handler (evil-yank-line-handler nil t) fontified t) 272
      273
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      273 274
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      274 286
      (yank-handler (evil-yank-line-handler nil t) fontified t) 286
      287
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      287 300
      (yank-handler (evil-yank-line-handler nil t) fontified t) 300
      301
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      301 334
      (yank-handler (evil-yank-line-handler nil t) fontified t) 334
      335
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      335 336
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      336 348
      (yank-handler (evil-yank-line-handler nil t) fontified t) 348
      349
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      349 369
      (yank-handler (evil-yank-line-handler nil t) fontified t) 369
      370
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      370 391
      (yank-handler (evil-yank-line-handler nil t) fontified t) 391
      392
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      392 393
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      393 405
      (yank-handler (evil-yank-line-handler nil t) fontified t) 405
      406
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      406 435
      (yank-handler (evil-yank-line-handler nil t) fontified t) 435
      436
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      436 438
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      438 439
      (yank-handler (evil-yank-line-handler nil t) fontified t) 439
      440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      440 444
      (yank-handler (evil-yank-line-handler nil t) fontified t) 444
      445
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      445 484
      (yank-handler (evil-yank-line-handler nil t) fontified t) 484
      485
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      485 486
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      486 503
      (yank-handler (evil-yank-line-handler nil t) fontified t) 503
      504
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      504 515
      (yank-handler (evil-yank-line-handler nil t) fontified t) 515
      516
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      516 522
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      522 523
      (yank-handler (evil-yank-line-handler nil t) fontified t) 523
      524
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      524 528
      (yank-handler (evil-yank-line-handler nil t) fontified t) 528
      529
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      529 530
      (yank-handler (evil-yank-line-handler nil t) fontified t) 530
      531
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      531 550
      (yank-handler (evil-yank-line-handler nil t) fontified t) 550
      551
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      551 552
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      552 572
      (yank-handler (evil-yank-line-handler nil t) fontified t) 572
      573
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      573 588
      (yank-handler (evil-yank-line-handler nil t) fontified t) 588
      589
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      589 592
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      592 593
      (yank-handler (evil-yank-line-handler nil t) fontified t) 593
      594
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      594 595
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      595 608
      (yank-handler (evil-yank-line-handler nil t) fontified t) 608
      609
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      609 610
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      610 627
      (yank-handler (evil-yank-line-handler nil t) fontified t) 627
      628
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      628 635
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      635 698
      (yank-handler (evil-yank-line-handler nil t) fontified t) 698
      699
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      699 702
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      702 703
      (yank-handler (evil-yank-line-handler nil t) fontified t) 703
      704
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      704 723
      (yank-handler (evil-yank-line-handler nil t) fontified t) 723
      724
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      724 760
      (yank-handler (evil-yank-line-handler nil t) fontified t) 760
      761
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      761 763
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      763 764
      (yank-handler (evil-yank-line-handler nil t) fontified t) 764
      765
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      765 778
      (yank-handler (evil-yank-line-handler nil t) fontified t) 778
      779
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      779 819
      (yank-handler (evil-yank-line-handler nil t) fontified t) 819
      820
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      820 828
      (yank-handler (evil-yank-line-handler nil t) fontified t) 828
      829
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      829 841
      (yank-handler (evil-yank-line-handler nil t) fontified t) 841
      842
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      842 843
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      843 844
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      844 845
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      845 911
      (yank-handler (evil-yank-line-handler nil t) fontified t) 911
      912
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      912 916
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      916 931
      (yank-handler (evil-yank-line-handler nil t) fontified t) 931
      932
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      932 933
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      933 934
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      934 935
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      935 936
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      936 937
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      937 945
      (yank-handler (evil-yank-line-handler nil t) fontified t) 945
      946
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      946 950
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      950 976
      (yank-handler (evil-yank-line-handler nil t) fontified t) 976
      977
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      977 987
      (yank-handler (evil-yank-line-handler nil t) fontified t) 987
      988
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      988 997
      (yank-handler (evil-yank-line-handler nil t) fontified t) 997
      998
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      998 999
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      999 1008
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1008
      1009
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1009 1022
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1022
      1023
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1023 1042
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1042
      1043
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1043 1044
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1044 1045
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1045 1046
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1046 1047
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1047 1048
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1048
      1049 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1049 1051
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1051
      1052
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1052 1057
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1057 1058
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1058
      1090
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1090 1091
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1091
      1092
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1092 1093
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1093 1094
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1094
      1098 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1098 1141
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      1141 1142
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1142
      1146 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1146 1147
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1147 1158
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1158 1159
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1159 1160
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1160
      1164 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1164 1165
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1165 1169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1169 1170
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1170
      1171
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1171 1172
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1172 1185
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1185
      1186
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1186 1204
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1204
      1205
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1205 1206
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1206 1207
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1207
      1218 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1218 1219
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1219 1232
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1232
      1233
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1233 1266
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1266
      1267
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1267 1268
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1268 1269
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1269
      1280 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1280 1281
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1281 1301
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1301
      1302
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1302 1323
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1323
      1324
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1324 1325
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1325 1326
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1326
      1337 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1337 1338
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1338 1359
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1359
      1371 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1371 1372
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1372 1374
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1374 1375
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1375
      1376
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1376 1380
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1380
      1381
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1381 1420
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1420
      1421
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1421 1422
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1422 1423
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1423
      1439 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1439 1440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1440 1451
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1451
      1452
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1452 1458
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1458 1459
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1459
      1460
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1460 1464
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1464
      1465
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1465 1466
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1466
      1467
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1467 1486
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1486
      1487
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1487 1488
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1488 1489
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1489
      1517 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1517 1518
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1518 1545
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1545
      1546
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1546 1547
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1547 1548
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1548
      1562 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1562 1563
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1563 1566
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1566 1567
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1567
      1568
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1568 1569
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1569 1586
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1586
      1587
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1587 1588
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1588 1589
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1589
      1605 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1605 1606
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1606 1612
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1612 1613
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1613
      1614
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1614 1638
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1638
      1639
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1639 1640
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1640
      1658 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1658 1659
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1659 1663
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1663 1664
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1664
      1665
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1665 1668
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1668 1669
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1669
      1670
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1670 1689
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1689
      1690
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1690 1691
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1691
      1720 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1720 1721
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1721 1725
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1725
      1726
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1726 1733
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1733
      1734
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1734 1741
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1741
      1742
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1742 1755
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1755
      1756
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1756 1757
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1757 1758
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1758 1759
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1759
      1779 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1779 1780
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1780 1784
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1784 1803
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1803
      1804
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1804 1805
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1805 1806
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1806 1807
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1807
      1836 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1836 1837
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1837 1838
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1838 1839
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1839 1840
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1840 1841
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1841
      1842 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1842 1848
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1848
      1849
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1849 1853
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1853 1875
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1875
      1883 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1883 1884
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1884 1894
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1894
      1895
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1895 1904
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1904
      1905
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1905 1906
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1906 1907
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1907
      1915 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1915 1916
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1916 1929
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1929
      1930
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1930 1953
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1953
      1954
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1954 1955
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1955 1956
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1956 1957
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1957 1958
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1958 1959
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1959
      1960 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1960 1962
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1962
      1963
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1963 1968
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1968 1969
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1969
      1999
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1999 2000
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2000
      2001
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2001 2002
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2002 2003
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2003
      2007 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2007 2051
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      2051 2052
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2052
      2056 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2056 2057
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2057 2067
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2067 2076
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2076
      2077
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2077 2078
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2078 2079
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2079 2080
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2080
      2084 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2084 2085
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2085 2089
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2089 2090
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2090
      2091
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2091 2092
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2092 2102
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2102
      2103
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2103 2119
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2119
      2120
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2120 2121
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2121 2122
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2122
      2133 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2133 2134
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2134 2147
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2147
      2155
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2155 2156
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2156 2157
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2157
      2168 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2168 2169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2169 2176
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2176
      2188 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2188 2189
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2189 2213
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2213
      2226 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2226 2227
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2227 2234
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2234
      2248 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2248 2332
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2332 2333
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2333
      2357 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2357 2383
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2383
      2384
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2384 2385
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2385 2386
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2386 2387
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2387 2388
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2388
      2389 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2389 2395
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2395
      2396
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2396 2406
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2406 2413
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2413
      2414
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2414 2434
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2434
      2438
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2438 2439
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2439 2440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2440 2441
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2441
      2442 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2442 2448
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2448
      2449
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2449 2455
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2455 2456
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2456
      2457
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2457 2467
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2467
      2468
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2468 2477
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2477
      2478
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2478 2482
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2482 2492
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2492
      2493
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2493 2494
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2494 2507
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2507
      2508
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2508 2510
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2510 2511
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2511 2518
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2518 2520
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2520 2521
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2521 2522
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2522 2524
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2524 2525
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2525 2531
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2531 2533
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2533 2534
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2534 2536
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2536 2539
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2539 2541
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2541 2543
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2543 2544
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2544 2547
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2547 2552
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2552
      2553
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2553 2564
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2564
      2565
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2565 2568
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2568 2569
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2569
      2570
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2570 2571
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2571 2575
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2575
      2576
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2576 2593
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2593
      2594
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2594 2613
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2613
      2614
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2614 2615
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2615 2616
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2616 2633
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2633
      2634
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2634 2640
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2640
      2641
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2641 2658
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2658
      2659
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2659 2678
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2678
      2679
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2679 2680
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2680 2681
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2681 2682
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2682 2695
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2695
      2696
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2696 2698
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2698 2699
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2699
      2700
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2700 2709
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2709
      2710
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2710 2727
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2727
      2728
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2728 2748
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2748
      2749
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2749 2754
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2754
      2755
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2755 2762
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2762
      2763
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2763 2764
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2764
      2773
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2773 2774
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2774 2775
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2775 2790
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2790
      2791
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2791 2798
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2798 2799
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2799
      2800
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2800 2807
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2807
      2808
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2808 2825
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2825
      2826
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2826 2846
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2846
      2847
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2847 2852
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2852
      2853
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2853 2860
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2860
      2861
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2861 2862
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2862
      2871
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2871 2872
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2872 2873
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2873 2874
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2874 2875
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2875 2876
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2876 2877
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2877 2888
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2888
      2889
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2889 2890
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2890 2903
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2903
      2904
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2904 2906
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2906 2907
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2907 2914
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2914 2916
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      2916 2917
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      2917 2919
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2919 2924
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2924
      2925
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2925 2936
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2936
      2937
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2937 2957
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2957
      2958
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2958 2963
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2963
      2964
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2964 2985
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2985
      2986
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2986 2987
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2987
      2994
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2994 2995
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2995 2996
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2996 2997
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2997 2998
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2998 2999
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2999 3007
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3007
      3008
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3008 3018
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3018 3046
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3046
      3047
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3047 3060
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3060
      3061
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3061 3062
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3062 3066
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3066
      3067
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3067 3072
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3072 3073
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3073
      3095
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3095 3096
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3096
      3097
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3097 3102
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3102 3104
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3104
      3105
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3105 3110
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3110
      3137
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      3137 3138
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3138
      3142 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3142 3143
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3143 3162
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3162
      3163
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3163 3178
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3178
      3179
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3179 3183
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3183
      3184
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3184 3185
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3185
      3186 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3186 3190
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3190
      3191
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3191 3207
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3207
      3208
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3208 3217
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3217
      3218
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3218 3219
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3219
      3220
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3220 3229
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3229
      3230
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3230 3231
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3231
      3277 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3277 3278
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3278 3279
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3279
      3280 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3280 3284
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3284
      3285
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3285 3288
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3288 3289
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3289
      3290
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3290 3291
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      3291 3304
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3304
      3305
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3305 3307
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3307 3308
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3308
      3309
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3309 3339
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3339
      3340
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3340 3342
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3342
      3343
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3343 3344
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3344 3345
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3345 3346
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      3346 3347
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3347 3348
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3348
      3354 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3354 3355
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3355 3369
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3369 3370
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3370
      3378 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3378 3379
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      3379 3385
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3385 3386
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3386
      3387
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3387 3411
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3411
      3412
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3412 3413
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3413
      3423 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3423 3424
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3424 3427
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3427 3428
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3428
      3429
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3429 3430
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3430 3439
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3439
      3440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3440 3455
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3455
      3456
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3456 3457
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3457 3458
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3458
      3474 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3474 3475
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3475 3482
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3482
      3483
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3483 3498
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3498
      3499
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3499 3500
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3500 3501
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3501 3502
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3502
      3514 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3514 3515
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3515 3525
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3525
      3526
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3526 3535
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3535
      3536
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3536 3537
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3537 3538
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3538
      3550 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3550 3551
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3551 3564
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3564
      3565
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3565 3576
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3576
      3577
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3577 3578
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3578 3579
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3579
      3580 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3580 3592
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3592
      3593
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3593 3596
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      3596 3597
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3597
      3598
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3598 3599
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3599 3607
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3607
      3608
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3608 3621
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3621
      3622
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3622 3634
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3634
      3635
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3635 3636
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3636
      3637
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3637 3649
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3649
      3650
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3650 3651
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3651 3652
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3652 3653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3653 3654
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3654
      3668 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3668 3669
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3669 3714
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3714
      3715
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3715 3716
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3716
      3730 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3730 3731
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      3731 3766
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3766
      3793 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3793 3794
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3794 3805
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3805
      3808
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      3808 3809
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3809
      3857 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3857 3897
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3897
      3898
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      3898 3899
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3899 3917
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3917
      3918
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3918 3919
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3919
      3961 (yank-handler (evil-yank-line-handler nil t) fontified t)
      3961 3962
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      3962 3973
      (yank-handler (evil-yank-line-handler nil t) fontified t) 3973
      4016 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4016 4017
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4017 4022
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4022 4030
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4030
      4075 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4075 4076
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4076 4083
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4083 4084
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4084
      4087
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4087 4088
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4088 4089
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4089
      4134 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4134 4135
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4135 4144
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4144 4145
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4145
      4148
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4148 4149
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4149 4150
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4150
      4195 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4195 4196
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4196 4205
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4205 4206
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4206
      4209
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4209 4210
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4210 4211
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4211 4212
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4212
      4262 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4262 4263
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4263 4274
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4274 4275
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4275
      4281
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4281 4282
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4282 4283
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4283 4284
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4284 4285
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4285 4286
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      4286 4287
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4287 4288
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4288 4289
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      4289 4290
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4290 4291
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4291 4292
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4292 4293
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4293
      4294 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4294 4296
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4296
      4297
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4297 4302
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4302 4303
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4303
      4319
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4319 4320
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4320
      4321
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4321 4322
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4322 4323
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4323
      4327 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4327 4328
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4328 4339
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4339 4340
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4340 4341
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4341
      4345 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4345 4346
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4346 4368
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4368
      4369
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4369 4370
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4370
      4374 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4374 4375
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4375 4424
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4424
      4425
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4425 4426
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4426
      4430 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4430 4431
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4431 4481
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4481
      4482
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4482 4483
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4483 4484
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4484
      4485 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4485 4487
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4487
      4488
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4488 4493
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4493 4494
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4494
      4511
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4511 4512
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4512
      4513
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4513 4514
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4514 4515
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4515
      4519 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4519 4520
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4520 4531
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      4531 4532
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4532 4533
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4533
      4537 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4537 4538
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4538 4554
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4554
      4555
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4555 4564
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4564
      4565
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4565 4566
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4566
      4567
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4567 4576
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4576
      4577
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      4577 4578
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4578
      4624 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4624 4625
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4625 4626
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4626
      4630 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4630 4631
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4631 4683
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4683
      4684
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4684 4685
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4685
      4689 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4689 4690
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4690 4743
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4743
      4744
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      4744 4745
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4745 4746
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4746
      4747 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4747 4749
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4749
      4752
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      4752 4764
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      4764 4766
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4766
      4767
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4767 4785
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4785
      4790
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4790 4825
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4825
      4826
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4826 4827
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4827
      4829 (yank-handler (evil-yank-line-handler nil t) fontified t)
      4829 4830
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4830 4848
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4848
      4853
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4853 4884
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4884
      4885
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4885 4889
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4889
      4890
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4890 4908
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4908
      4917
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4917 4952
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4952
      4953
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4953 4956
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4956
      4957
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      4957 4975
      (yank-handler (evil-yank-line-handler nil t) fontified t) 4975
      4984
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      4984 5015
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5015
      5016
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5016 5019
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5019
      5020
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5020 5038
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5038
      5047
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      5047 5066
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5066
      5067
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5067 5070
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5070
      5071
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5071 5089
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5089
      5098
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      5098 5118
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5118
      5119
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5119 5123
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5123
      5124
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5124 5128
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      5128 5165
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5165
      5166
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      5166 5210
      (yank-handler (evil-yank-line-handler nil t) fontified t) 5210
      5211
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      5211 5212
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      5212 5213
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      5213 5214
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
