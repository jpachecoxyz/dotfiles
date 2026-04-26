;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((114)
 (105
  . [101 120 105 116 return escape escape 105 101 120 105 116 return
         101 120 105 116 return escape])
 (97) (113)
 (48
  . #(";;; Evil-mode\n\n(jp-emacs-configure\n  (jp-emacs-install evil)\n  (evil-mode 1))\n\n  (defun jp/org-tab-dwim ()\n    (interactive)\n      (or (yas-expand)\n        (org-cycle)))\n\n  (with-eval-after-load 'org\n    (evil-define-key '(normal insert) org-mode-map\n        (kbd \"TAB\") #'jp/org-tab-dwim\n        (kbd \"<backtab>\") #'org-shifttab))\n\n  (with-eval-after-load 'evil\n    (evil-define-key '(normal insert) lisp-interaction-mode-map\n        (kbd \"C-j\") #'jp-elisp-eval-and-print-last-sexp))\n\n  (setq evil-want-integration t)  ;; This is optional since it's already set to t by default.\n  (setq evil-want-keybinding nil)\n  (setq evil-vsplit-window-right t)\n  (setq evil-split-window-below t)\n  (setq evil-mode-line-format nil)\n  (setq evil-undo-system 'undo-redo) \n\n(jp-emacs-configure\n  (jp-emacs-install evil-collection))\n\n(jp-emacs-configure\n  (jp-emacs-install evil-nerd-commenter))\n\n(jp-emacs-configure\n  (jp-emacs-install evil-numbers))\n\n(jp-emacs-configure\n  (jp-emacs-install evil-surround))\n\n(with-eval-after-load 'evil\n  ;; (add-to-list 'evil-collection-mode-list 'help)\n  (evil-collection-init)\n\n  (global-evil-surround-mode 1)\n\n  (define-key evil-normal-state-map (kbd \"C-a\") #'evil-numbers/inc-at-pt)\n  (define-key evil-normal-state-map (kbd \"C-s\") #'evil-numbers/dec-at-pt))\n\n;; Using RETURN to follow links in Org/Evil\n(with-eval-after-load 'evil-maps\n  (define-key evil-motion-state-map (kbd \"SPC\") nil)\n  (define-key evil-motion-state-map (kbd \"RET\") nil)\n  (define-key evil-motion-state-map (kbd \"TAB\") nil))\n\n ;; Dired vim-like navigation\n(with-eval-after-load 'dired\n(evil-define-key 'normal dired-mode-map\n    (kbd \"h\") #'dired-up-directory\n    (kbd \"l\") #'dired-find-file))\n\n(provide 'jp-emacs-evil)\n"
      0 4
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      4 14
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      14 15 (yank-handler (evil-yank-line-handler nil t) fontified t)
      15 16
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      16 34
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      34 37 (yank-handler (evil-yank-line-handler nil t) fontified t)
      37 38
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      38 54
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      54 59 (yank-handler (evil-yank-line-handler nil t) fontified t)
      59 60
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      60 63 (yank-handler (evil-yank-line-handler nil t) fontified t)
      63 64
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      64 75 (yank-handler (evil-yank-line-handler nil t) fontified t)
      75 76
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      76 77
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      77 81 (yank-handler (evil-yank-line-handler nil t) fontified t)
      81 82
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      82 87
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      87 88 (yank-handler (evil-yank-line-handler nil t) fontified t)
      88 103
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      103 104
      (yank-handler (evil-yank-line-handler nil t) fontified t) 104
      105
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      105 106
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      106 111
      (yank-handler (evil-yank-line-handler nil t) fontified t) 111
      112
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      112 123
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      123 124
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      124 131
      (yank-handler (evil-yank-line-handler nil t) fontified t) 131
      132
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      132 134
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      134 135
      (yank-handler (evil-yank-line-handler nil t) fontified t) 135
      136
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      136 146
      (yank-handler (evil-yank-line-handler nil t) fontified t) 146
      147
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      147 156
      (yank-handler (evil-yank-line-handler nil t) fontified t) 156
      157
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      157 166
      (yank-handler (evil-yank-line-handler nil t) fontified t) 166
      167
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      167 168
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      168 169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      169 173
      (yank-handler (evil-yank-line-handler nil t) fontified t) 173
      174
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      174 194
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      194 204
      (yank-handler (evil-yank-line-handler nil t) fontified t) 204
      205
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      205 220
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      220 222
      (yank-handler (evil-yank-line-handler nil t) fontified t) 222
      223
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      223 236
      (yank-handler (evil-yank-line-handler nil t) fontified t) 236
      237
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      237 259
      (yank-handler (evil-yank-line-handler nil t) fontified t) 259
      260
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      260 264
      (yank-handler (evil-yank-line-handler nil t) fontified t) 264
      269
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      269 270
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      270 297
      (yank-handler (evil-yank-line-handler nil t) fontified t) 297
      298
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      298 302
      (yank-handler (evil-yank-line-handler nil t) fontified t) 302
      313
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      313 314
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      314 329
      (yank-handler (evil-yank-line-handler nil t) fontified t) 329
      330
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      330 331
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      331 335
      (yank-handler (evil-yank-line-handler nil t) fontified t) 335
      336
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      336 356
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      356 367
      (yank-handler (evil-yank-line-handler nil t) fontified t) 367
      368
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      368 383
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      383 385
      (yank-handler (evil-yank-line-handler nil t) fontified t) 385
      386
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      386 399
      (yank-handler (evil-yank-line-handler nil t) fontified t) 399
      400
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      400 435
      (yank-handler (evil-yank-line-handler nil t) fontified t) 435
      436
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      436 440
      (yank-handler (evil-yank-line-handler nil t) fontified t) 440
      445
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      445 446
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      446 482
      (yank-handler (evil-yank-line-handler nil t) fontified t) 482
      483
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      483 484
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      484 488
      (yank-handler (evil-yank-line-handler nil t) fontified t) 488
      489
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      489 493
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      493 517
      (yank-handler (evil-yank-line-handler nil t) fontified t) 517
      518
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      518 520
      (yank-handler (evil-yank-line-handler nil t) fontified t) 520
      523
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      523 580
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      580 582
      (yank-handler (evil-yank-line-handler nil t) fontified t) 582
      583
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      583 587
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      587 612
      (yank-handler (evil-yank-line-handler nil t) fontified t) 612
      613
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      613 616
      (yank-handler (evil-yank-line-handler nil t) fontified t) 616
      617
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      617 621
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      621 648
      (yank-handler (evil-yank-line-handler nil t) fontified t) 648
      649
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      649 652
      (yank-handler (evil-yank-line-handler nil t) fontified t) 652
      653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      653 657
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      657 683
      (yank-handler (evil-yank-line-handler nil t) fontified t) 683
      684
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      684 687
      (yank-handler (evil-yank-line-handler nil t) fontified t) 687
      688
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      688 692
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      692 718
      (yank-handler (evil-yank-line-handler nil t) fontified t) 718
      719
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      719 722
      (yank-handler (evil-yank-line-handler nil t) fontified t) 722
      723
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      723 727
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      727 755
      (yank-handler (evil-yank-line-handler nil t) fontified t) 755
      756
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      756 759
      (yank-handler (evil-yank-line-handler nil t) fontified t) 759
      760
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      760 778
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      778 781
      (yank-handler (evil-yank-line-handler nil t) fontified t) 781
      782
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      782 798
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      798 814
      (yank-handler (evil-yank-line-handler nil t) fontified t) 814
      815
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      815 816
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      816 818
      (yank-handler (evil-yank-line-handler nil t) fontified t) 818
      819
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      819 837
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      837 840
      (yank-handler (evil-yank-line-handler nil t) fontified t) 840
      841
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      841 857
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      857 877
      (yank-handler (evil-yank-line-handler nil t) fontified t) 877
      878
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      878 879
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      879 881
      (yank-handler (evil-yank-line-handler nil t) fontified t) 881
      882
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      882 900
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      900 903
      (yank-handler (evil-yank-line-handler nil t) fontified t) 903
      904
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      904 920
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      920 933
      (yank-handler (evil-yank-line-handler nil t) fontified t) 933
      934
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      934 935
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      935 937
      (yank-handler (evil-yank-line-handler nil t) fontified t) 937
      938
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      938 956
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      956 959
      (yank-handler (evil-yank-line-handler nil t) fontified t) 959
      960
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      960 976
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      976 990
      (yank-handler (evil-yank-line-handler nil t) fontified t) 990
      991
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      991 992
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      992 994
      (yank-handler (evil-yank-line-handler nil t) fontified t) 994
      995
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      995 1015
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1015 1024
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1024
      1027
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      1027 1074
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1074 1076
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1076
      1077
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1077 1097
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1097
      1098
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1098 1102
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1102
      1103
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1103 1130
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1130
      1131
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1131 1135
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1135
      1136
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1136 1169
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1169
      1170
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1170 1174
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1174
      1179
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1179 1180
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1180 1205
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1205
      1206
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1206 1209
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1209
      1210
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1210 1243
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1243
      1244
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1244 1248
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1248
      1253
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1253 1254
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1254 1279
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1279
      1280
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1280 1281
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1281 1283
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1283
      1286
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      1286 1327
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1327 1328
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1328 1348
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1348 1362
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1362
      1363
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1363 1396
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1396
      1397
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1397 1401
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1401
      1406
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1406 1407
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1407 1411
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1411
      1412
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1412 1415
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1415
      1416
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1416 1449
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1449
      1450
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1450 1454
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1454
      1459
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1459 1460
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1460 1464
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1464
      1465
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1465 1468
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1468
      1469
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1469 1500
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1500
      1502 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1502 1503
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1503 1507
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1507
      1512
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1512 1513
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1513 1517
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1517
      1518
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1518 1519
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified nil)
      1519 1522
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1522
      1525
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-delimiter-face)
      1525 1551
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-comment-face)
      1551 1552
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1552 1572
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      1572 1580
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1580
      1581
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      1581 1596
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      1596 1624
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1624
      1625
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1625 1629
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1629
      1632
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      1632 1633
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1633 1659
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1659
      1660
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1660 1664
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1664
      1667
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-string-face)
      1667 1668
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-variable-name-face)
      1668 1686
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1686
      1687
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-function-name-face)
      1687 1688
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1688 1690
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1690
      1691
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1691 1698
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-keyword-face)
      1698 1700
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1700
      1713
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-constant-face)
      1713 1714
      (yank-handler (evil-yank-line-handler nil t) fontified nil face
                    font-lock-type-face)
      1714 1715
      (yank-handler (evil-yank-line-handler nil t) fontified nil))))
