;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #("  (defun my/org-notify-telegram-action-with-chat (notif-plist)\n    (let* ((heading  (or (plist-get notif-plist :heading) \"Tarea pendiente\"))\n           (deadline (plist-get notif-plist :deadline))\n           (sched    (plist-get notif-plist :scheduled))\n           (file     (file-name-nondirectory\n                      (or (plist-get notif-plist :file) \"~/Documents/Emacs/org/agenda/agenda.org\")))\n           (marker   (plist-get notif-plist :marker))\n           (chat-id\n            (when marker\n              (org-with-point-at marker\n                (let ((limit (save-excursion (outline-next-heading) (point))))\n                  (when (re-search-forward\n                         \"^[[:space:]]*:TELEGRAM_ID:[[:space:]]+\\\\(-?[0-9]+\\\\)\" limit t)\n                    (string-to-number (match-string-no-properties 1)))))))\n           (msg (concat\n                 \"📅 *Tarea pendiente*\\n\\n\"\n                 (format \"• *Tarea:* %s\\n\" heading)\n                 (when deadline\n                   (format \"• *Fecha límite:* %s\\n\"\n                           (format-time-string \"%d/%m %H:%M\" deadline)))\n                 (when sched\n                   (format \"• *Programada:* %s\\n\"\n                           (format-time-string \"%d/%m %H:%M\" sched)))\n                 (format \"• *Archivo:* %s\" file))))\n      (message \"[tg] chat-id=%s heading=%s\" chat-id heading)\n      (when (and chat-id (featurep 'telega) (telega-server-live-p))\n        (run-at-time 0 nil\n                     `(lambda ()\n                        (condition-case err\n                            (progn\n                              (telega--sendMessage\n                               (telega-chat-get ,chat-id)\n                               (list :@type \"inputMessageText\"\n                                     :text (telega-string-fmt-text ,msg))\n                               nil nil)\n                              (message \"[tg] OK\"))\n                          (error\n                           (message \"[tg] ERROR: %s\"\n                                    (error-message-string err)))))))))\n  ;; Registrar nueva regla que use la acción con destinatario variable\n  (org-notify-add 'telegram-tasks-to-chat\n                  '(:time \"1m\"\n                          :period \"1m\"\n                          :duration 10\n                          :actions (my/org-notify-telegram-action-with-chat -notify/window -sound)))\n"
      0 2 (yank-handler (evil-yank-line-handler nil t) fontified t) 2
      3
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      3 8
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      8 9 (yank-handler (evil-yank-line-handler nil t) fontified t) 9
      48
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      48 49 (yank-handler (evil-yank-line-handler nil t) fontified t)
      49 50
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      50 61 (yank-handler (evil-yank-line-handler nil t) fontified t)
      61 62
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      62 67 (yank-handler (evil-yank-line-handler nil t) fontified t)
      67 68
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      68 72
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      72 73 (yank-handler (evil-yank-line-handler nil t) fontified t)
      73 74
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      74 75
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      75 84 (yank-handler (evil-yank-line-handler nil t) fontified t)
      84 85
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      85 87
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      87 88 (yank-handler (evil-yank-line-handler nil t) fontified t)
      88 89
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      89 111 (yank-handler (evil-yank-line-handler nil t) fontified t)
      111 119
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      119 120
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      120 121
      (yank-handler (evil-yank-line-handler nil t) fontified t) 121
      138
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      138 139
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      139 140
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      140 141
      (yank-handler (evil-yank-line-handler nil t) fontified t) 141
      152 (yank-handler (evil-yank-line-handler nil t) fontified t)
      152 153
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      153 162
      (yank-handler (evil-yank-line-handler nil t) fontified t) 162
      163
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      163 185
      (yank-handler (evil-yank-line-handler nil t) fontified t) 185
      194
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      194 195
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      195 196
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      196 197
      (yank-handler (evil-yank-line-handler nil t) fontified t) 197
      208 (yank-handler (evil-yank-line-handler nil t) fontified t)
      208 209
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      209 218
      (yank-handler (evil-yank-line-handler nil t) fontified t) 218
      219
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      219 241
      (yank-handler (evil-yank-line-handler nil t) fontified t) 241
      251
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      251 252
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      252 253
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      253 254
      (yank-handler (evil-yank-line-handler nil t) fontified t) 254
      265 (yank-handler (evil-yank-line-handler nil t) fontified t)
      265 266
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      266 275
      (yank-handler (evil-yank-line-handler nil t) fontified t) 275
      276
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      276 299
      (yank-handler (evil-yank-line-handler nil t) fontified t) 299
      321 (yank-handler (evil-yank-line-handler nil t) fontified t)
      321 322
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      322 324
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      324 325
      (yank-handler (evil-yank-line-handler nil t) fontified t) 325
      326
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      326 348
      (yank-handler (evil-yank-line-handler nil t) fontified t) 348
      353
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      353 354
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      354 355
      (yank-handler (evil-yank-line-handler nil t) fontified t) 355
      396
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      396 397
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      397 398
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      398 399
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      399 400
      (yank-handler (evil-yank-line-handler nil t) fontified t) 400
      411 (yank-handler (evil-yank-line-handler nil t) fontified t)
      411 412
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      412 421
      (yank-handler (evil-yank-line-handler nil t) fontified t) 421
      422
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      422 444
      (yank-handler (evil-yank-line-handler nil t) fontified t) 444
      451
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      451 452
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      452 453
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      453 454
      (yank-handler (evil-yank-line-handler nil t) fontified t) 454
      465 (yank-handler (evil-yank-line-handler nil t) fontified t)
      465 466
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      466 486
      (yank-handler (evil-yank-line-handler nil t) fontified t) 486
      487
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      487 491
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      491 499
      (yank-handler (evil-yank-line-handler nil t) fontified t) 499
      513 (yank-handler (evil-yank-line-handler nil t) fontified t)
      513 514
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      514 531
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      531 555
      (yank-handler (evil-yank-line-handler nil t) fontified t) 555
      556
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      556 559
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      559 560
      (yank-handler (evil-yank-line-handler nil t) fontified t) 560
      561
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      561 562
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      562 568
      (yank-handler (evil-yank-line-handler nil t) fontified t) 568
      569
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      569 583
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      583 584
      (yank-handler (evil-yank-line-handler nil t) fontified t) 584
      585
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      585 605
      (yank-handler (evil-yank-line-handler nil t) fontified t) 605
      606
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      606 607
      (yank-handler (evil-yank-line-handler nil t) fontified t) 607
      608
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      608 613
      (yank-handler (evil-yank-line-handler nil t) fontified t) 613
      614
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      614 615
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      615 616
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      616 617
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      617 618
      (yank-handler (evil-yank-line-handler nil t) fontified t) 618
      636 (yank-handler (evil-yank-line-handler nil t) fontified t)
      636 637
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      637 641
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      641 642
      (yank-handler (evil-yank-line-handler nil t) fontified t) 642
      643
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      643 661
      (yank-handler (evil-yank-line-handler nil t) fontified t) 661
      686 (yank-handler (evil-yank-line-handler nil t) fontified t)
      686 725
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      725 727
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      727 728
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      728 736
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      736 738
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-backslash
                     font-lock-string-face)
                    fontified t)
      738 739
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-regexp-grouping-construct
                     font-lock-string-face)
                    fontified t)
      739 740
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      740 748
      (yank-handler (evil-yank-line-handler nil t) fontified t) 748
      749
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      749 750
      (yank-handler (evil-yank-line-handler nil t) fontified t) 750
      770 (yank-handler (evil-yank-line-handler nil t) fontified t)
      770 771
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      771 788
      (yank-handler (evil-yank-line-handler nil t) fontified t) 788
      789
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      789 817
      (yank-handler (evil-yank-line-handler nil t) fontified t) 817
      818
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      818 819
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      819 820
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      820 821
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      821 822
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      822 823
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      823 824
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      824 825
      (yank-handler (evil-yank-line-handler nil t) fontified t) 825
      836 (yank-handler (evil-yank-line-handler nil t) fontified t)
      836 837
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      837 841
      (yank-handler (evil-yank-line-handler nil t) fontified t) 841
      842
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      842 866
      (yank-handler (evil-yank-line-handler nil t) fontified t) 866
      891
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      891 909
      (yank-handler (evil-yank-line-handler nil t) fontified t) 909
      910
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      910 917
      (yank-handler (evil-yank-line-handler nil t) fontified t) 917
      934
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      934 942
      (yank-handler (evil-yank-line-handler nil t) fontified t) 942
      943
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      943 961
      (yank-handler (evil-yank-line-handler nil t) fontified t) 961
      962
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      962 966
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      966 976
      (yank-handler (evil-yank-line-handler nil t) fontified t) 976
      995 (yank-handler (evil-yank-line-handler nil t) fontified t)
      995 996
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      996 1003
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1003
      1027
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1027 1028
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1028
      1055 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1055 1056
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1056 1075
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1075
      1088
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1088 1097
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1097
      1098
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1098 1099
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1099 1100
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1100 1118
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1118
      1119
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1119 1123
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1123 1130
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1130
      1142 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1142 1149
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1149
      1150
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1150 1157
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1157
      1179
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1179 1180
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1180
      1207 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1207 1208
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1208 1227
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1227
      1240
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1240 1246
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1246
      1247
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1247 1248
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1248 1249
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1249 1267
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1267
      1268
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1268 1275
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1275
      1292
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1292 1297
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1297
      1298
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1298 1299
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1299 1300
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1300 1301
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1301 1302
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1302
      1308 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1308 1309
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1309 1317
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1317
      1345
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1345 1351
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1351
      1361 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1361 1362
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1362 1363
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1363
      1369 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1369 1370
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1370 1374
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1374 1375
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1375
      1376
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1376 1379
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1379 1388
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1388
      1389
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1389 1397
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1397 1399
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1399
      1405
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1405 1406
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1406 1407
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1407
      1408
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1408 1423
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1423
      1428 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1428 1429
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1429 1430
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1430 1431
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1431
      1439 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1439 1440
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1440 1452
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1452
      1455 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1455 1479
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1479
      1480 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1480 1481
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1481 1487
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1487 1488
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1488
      1489
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1489 1490
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1490 1491
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1491
      1515 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1515 1516
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1516 1527
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1527 1530
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1530 1535
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1535
      1563 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1563 1564
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1564 1569
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      1569 1570
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1570
      1586 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1586 1600
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1600
      1601
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1601 1621
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1621
      1652 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1652 1653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1653 1677
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1677
      1678
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1678 1679
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1679
      1710 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1710 1711
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1711 1716
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1716
      1722
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1722 1723
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1723
      1741
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1741 1742
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1742
      1779 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1779 1784
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      1784 1785
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1785
      1786
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1786 1813
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1813
      1814
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1814 1815
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1815 1847
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1847
      1854 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1854 1855
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1855 1886
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1886
      1887
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1887 1895
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1895
      1900
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1900 1904
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1904 1905
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1905 1906
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1906 1933
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1933
      1934
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      1934 1939
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-warning-face fontified t)
      1939 1940
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1940
      1960 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1960 1967
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1967
      1968
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      1968 1976
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1976
      1992
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1992 2029
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2029
      2030
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2030 2054
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2054
      2055
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2055 2056
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2056 2057
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2057 2058
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2058 2059
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2059 2060
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2060 2061
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2061 2062
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2062 2063
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2063 2066
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2066
      2069
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2069 2135
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2135 2137
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2137
      2138
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2138 2177
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2177
      2195 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2195 2196
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2196
      2197
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2197 2202
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2202 2203
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2203
      2207
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2207 2234
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2234
      2241
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2241 2242
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2242
      2246
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2246 2247
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2247
      2250 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2250 2273
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2273
      2282
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2282 2312
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2312
      2320
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified t)
      2320 2321
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2321
      2322
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2322 2383
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2383
      2384
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2384 2385
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2385 2386
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2386 2387
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
