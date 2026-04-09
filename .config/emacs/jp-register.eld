;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((97) (113)
 (48
  . #("    (setq org-capture-templates\n          (let* ((without-time (concat \":PROPERTIES:\\n\"\n                                       \":CAPTURED: %U\\n\"\n                                       \":CUSTOM_ID: h:%(format-time-string \\\"%Y%m%dT%H%M%S\\\")\\n\"\n                                       \":END:\\n\\n\"\n                                       \"%a\\n%?\"))\n                 (with-time (concat \"DEADLINE: %^T\\n\"\n                                    \":PROPERTIES:\\n\"\n                                    \":CAPTURED: %U\\n\"\n                                    \":CUSTOM_ID: h:%(format-time-string \\\"%Y%m%dT%H%M%S\\\")\\n\"\n                                    \":APPT_WARNTIME: 20\\n\"\n                                    \":END:\\n\\n\"\n                                    \"%a%?\")))\n            `((\"s\" \"Select file and heading to add to\" entry\n               (function jp-org-select-heading-in-file)\n               ,(concat \"* TODO %^{Title}%?\\n\" without-time)\n               :empty-lines-after 1)\n              (\"t\" \"Task to do\" entry\n               (file+headline \"~/Documents/Emacs/org/agenda/refile.org\" \"All tasks\")\n               ,(concat \"* TODO %^{Title} \\n\" without-time)\n               :empty-lines-after 1)\n              ;; (\"·\" \"\") ; HACK to get a divider\n              (\"a\" \"Appointment\" entry\n               (file+headline \"~/Documents/Emacs/org/agenda/refile.org\" \"Appointments\")\n               ,(concat \"* TODO %^{Title}\\n\" with-time)\n               :empty-lines-after 1))))))\n"
      0 4 (yank-handler (evil-yank-line-handler nil t) fontified t) 4
      5
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      5 9
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      9 32 (yank-handler (evil-yank-line-handler nil t) fontified t)
      32 42 (yank-handler (evil-yank-line-handler nil t) fontified t)
      42 43
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      43 47
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      47 48 (yank-handler (evil-yank-line-handler nil t) fontified t)
      48 49
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      49 50
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      50 63 (yank-handler (evil-yank-line-handler nil t) fontified t)
      63 64
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      64 71 (yank-handler (evil-yank-line-handler nil t) fontified t)
      71 87
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      87 88 (yank-handler (evil-yank-line-handler nil t) fontified t)
      88 127 (yank-handler (evil-yank-line-handler nil t) fontified t)
      127 144
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      144 145
      (yank-handler (evil-yank-line-handler nil t) fontified t) 145
      184 (yank-handler (evil-yank-line-handler nil t) fontified t)
      184 241
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      241 242
      (yank-handler (evil-yank-line-handler nil t) fontified t) 242
      281 (yank-handler (evil-yank-line-handler nil t) fontified t)
      281 292
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      292 293
      (yank-handler (evil-yank-line-handler nil t) fontified t) 293
      332 (yank-handler (evil-yank-line-handler nil t) fontified t)
      332 340
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      340 341
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      341 342
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      342 343
      (yank-handler (evil-yank-line-handler nil t) fontified t) 343
      360 (yank-handler (evil-yank-line-handler nil t) fontified t)
      360 361
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      361 371
      (yank-handler (evil-yank-line-handler nil t) fontified t) 371
      372
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      372 379
      (yank-handler (evil-yank-line-handler nil t) fontified t) 379
      396
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      396 397
      (yank-handler (evil-yank-line-handler nil t) fontified t) 397
      433 (yank-handler (evil-yank-line-handler nil t) fontified t)
      433 449
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      449 450
      (yank-handler (evil-yank-line-handler nil t) fontified t) 450
      486 (yank-handler (evil-yank-line-handler nil t) fontified t)
      486 503
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      503 504
      (yank-handler (evil-yank-line-handler nil t) fontified t) 504
      540 (yank-handler (evil-yank-line-handler nil t) fontified t)
      540 597
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      597 598
      (yank-handler (evil-yank-line-handler nil t) fontified t) 598
      634 (yank-handler (evil-yank-line-handler nil t) fontified t)
      634 656
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      656 657
      (yank-handler (evil-yank-line-handler nil t) fontified t) 657
      693 (yank-handler (evil-yank-line-handler nil t) fontified t)
      693 704
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      704 705
      (yank-handler (evil-yank-line-handler nil t) fontified t) 705
      741 (yank-handler (evil-yank-line-handler nil t) fontified t)
      741 747
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      747 748
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      748 749
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      749 750
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      750 751
      (yank-handler (evil-yank-line-handler nil t) fontified t) 751
      764 (yank-handler (evil-yank-line-handler nil t) fontified t)
      764 765
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      765 766
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      766 769
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      769 770
      (yank-handler (evil-yank-line-handler nil t) fontified t) 770
      805
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      805 812
      (yank-handler (evil-yank-line-handler nil t) fontified t) 812
      827 (yank-handler (evil-yank-line-handler nil t) fontified t)
      827 828
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      828 836
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      836 866
      (yank-handler (evil-yank-line-handler nil t) fontified t) 866
      867
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      867 868
      (yank-handler (evil-yank-line-handler nil t) fontified t) 868
      884 (yank-handler (evil-yank-line-handler nil t) fontified t)
      884 885
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      885 892
      (yank-handler (evil-yank-line-handler nil t) fontified t) 892
      914
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      914 927
      (yank-handler (evil-yank-line-handler nil t) fontified t) 927
      928
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      928 929
      (yank-handler (evil-yank-line-handler nil t) fontified t) 929
      944 (yank-handler (evil-yank-line-handler nil t) fontified t)
      944 962
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      962 964
      (yank-handler (evil-yank-line-handler nil t) fontified t) 964
      965
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      965 966
      (yank-handler (evil-yank-line-handler nil t) fontified t) 966
      980 (yank-handler (evil-yank-line-handler nil t) fontified t)
      980 981
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      981 984
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      984 985
      (yank-handler (evil-yank-line-handler nil t) fontified t) 985
      997
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      997 1004
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1004
      1019 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1019 1020
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1020 1034
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1034
      1035
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1035 1073
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1073 1074
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face rear-nonsticky t fontified t)
      1074 1075
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1075 1076
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1076
      1087
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1087 1088
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1088 1089
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1089
      1105 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1105 1106
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1106 1113
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1113
      1134
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1134 1147
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1147
      1148
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1148 1149
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1149
      1164 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1164 1182
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1182 1184
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1184
      1185
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1185 1186
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1186
      1200 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1200 1203
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      1203 1204
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1204 1207
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1207 1208
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1208 1210
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1210 1211
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1211 1212
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1212 1214
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1214 1221
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1221 1235
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1235 1236
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      1236 1250
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1250
      1251
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1251 1254
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1254 1255
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1255
      1268
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1268 1275
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1275
      1290 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1290 1291
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1291 1305
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1305
      1335
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1335 1336
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1336 1346
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1346 1347
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1347
      1361
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1361 1362
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1362 1363
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1363
      1379 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1379 1380
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1380 1387
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1387
      1407
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1407 1417
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1417
      1418
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1418 1419
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1419
      1434 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1434 1452
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1452 1454
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1454
      1455
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1455 1456
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1456 1457
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1457 1458
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1458 1459
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1459 1460
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1460 1461
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
