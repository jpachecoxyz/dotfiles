;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((48
  . #(";;; jp-emacs-code.el\n;; Programming stufgf like LSP Clientes, Indents, etc.\n\n(jp-emacs-configure\n  (jp-emacs-install aggressive-indent)\n  (aggressive-indent-global-mode))\n\n;; LSP configuration\n(jp-emacs-configure\n\n  ;; eglot settings\n  (setq eglot-autoshutdown t\n        eglot-events-buffer-size 0\n        eglot-events-buffer-config '(:size 0 :format full)\n        eglot-prefer-plaintext t\n        eglot-code-action-indications nil\n        jsonrpc-event-hook nil)\n\n  ;; disable jsonrpc logging\n  (fset #'jsonrpc--log-event #'ignore)\n\n  ;; workspace config example\n  (setq-default\n   eglot-workspace-configuration\n   '(:gopls (:hints (:parameterNames t))))\n\n  ;; setup function\n  (defun jp/eglot-setup ()\n    \"Enable Eglot in programming modes except Emacs Lisp.\"\n    (unless (derived-mode-p 'emacs-lisp-mode)\n      (eglot-ensure)))\n\n  ;; Add lsp servers to eglot\n  (with-eval-after-load 'eglot\n    ;; Bash LSP, Starts 'bash-language-server' with 'start' argument.\n    ;; Example: $ bash-language-server start\n    (add-to-list 'eglot-server-programs\n                 '(sh-mode \"bash-language-server\" \"start\"))\n\n    ;; Python LSP\n    (add-to-list 'eglot-server-programs\n                 '(python-mode \"pylsp\"))\n\n    ;; Lua LSP\n    (add-to-list 'eglot-server-programs\n                 '(python-mode \"lua-language-server\"))\n\n    ;;; Typst LSP\n    (add-to-list 'eglot-server-programs\n                 `((typst-ts-mode) .\n                   ,(eglot-alternatives `(,typst-ts-lsp-download-path\n                                          \"tinymist\"\n                                          \"typst-lsp\"))))\n\n    )\n\n\n  ;; hooks\n  (add-hook 'prog-mode-hook #'jp/eglot-setup)\n  (add-hook 'shell-mode-hook #'eglot-ensure)\n  (add-hook 'typst-ts-mode-hook #'eglot-ensure)\n\n  ;; keybindings\n  (with-eval-after-load 'eglot\n    (define-key eglot-mode-map (kbd \"C-c l a\") #'eglot-code-actions)\n    (define-key eglot-mode-map (kbd \"C-c l o\") #'eglot-code-action-organize-imports)\n    (define-key eglot-mode-map (kbd \"C-c l r\") #'eglot-rename)\n    (define-key eglot-mode-map (kbd \"C-c l f\") #'eglot-format)))\n\n;;; Python\n;; by default, the function 'python-mode is associated with\n;; the package python.el. The following changes that to python-mode.el:\n(autoload 'python-mode \"python-mode\" \"Python Mode.\" t)\n\n;; open py files with python-mode\n(add-to-list 'auto-mode-alist '(\"\\\\.py\\\\'\" . python-mode))\n\n;; sets python interpreter mode to be python-mode\n(add-to-list 'interpreter-mode-alist '(\"python\" . python-mode))\n\n;;; Typst-mode\n(jp-emacs-configure\n  (jp-emacs-install typst-ts-mode \"https://codeberg.org/meow_king/typst-ts-mode\")\n\n  (setq typst-ts-watch-options \"--open\")\n\n  ;; Esperamos a que el modo cargue para definir sus teclas\n  (with-eval-after-load 'typst-ts-mode\n    (keymap-set typst-ts-mode-map \"C-c C-c\" #'typst-ts-tmenu)))\n\n;;; Lua \n(jp-emacs-configure\n  (jp-emacs-install lua-mode))\n\n(provide 'jp-emacs-code)\n"
      0 4
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      4 21
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      21 24
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      24 76
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      76 77
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 77
      78
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified nil)
      78 96
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      96 99
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 99
      100
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      100 116
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      116 134
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 134
      135
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      135 138
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 138
      139
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      139 168
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 168
      169
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      169 170
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified nil)
      170 172
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 172
      175
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      175 193
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      193 194
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified nil)
      194 212
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      212 216
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 216
      219
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      219 234
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      234 236
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 236
      237
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      237 241
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      241 334
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 334
      335
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      335 340
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      340 343
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 343
      350
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      350 355
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 355
      356
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      356 462
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 462
      463
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      463 467
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 467
      470
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      470 494
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      494 496
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 496
      497
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      497 531
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 531
      532
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      532 536
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 536
      539
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      539 564
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      564 566
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 566
      567
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      567 579
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      579 617
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 617
      618
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      618 624
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      624 625
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 625
      626
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      626 632
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      632 633
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 633
      634
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      634 649
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      649 651
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 651
      652
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      652 653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      653 654
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      654 655
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      655 656
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 656
      657 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      657 659
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 659
      662
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      662 677
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      677 679
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 679
      680
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      680 685
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      685 686
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 686
      700
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      700 701
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 701
      702
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      702 703
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      703 704
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 704
      708 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      708 762
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified nil)
      762 763
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 763
      767 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      767 768
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      768 774
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      774 775
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 775
      776
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      776 807
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 807
      808
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      808 809
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 809
      815 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      815 816
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      816 828
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 828
      829
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      829 830
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      830 831
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      831 832
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 832
      833 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      833 835
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 835
      838
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      838 863
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      863 865
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 865
      866
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      866 886
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      886 894
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 894
      898 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      898 901
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      901 919
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      919 939
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-comment-face)
                    fontified nil)
      939 947
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      947 952
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-comment-face)
                    fontified nil)
      952 964
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      964 968
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 968
      971
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      971 1009
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1009 1013
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1013
      1014
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1014 1049
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1049
      1067 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1067 1068
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1068 1076
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1076
      1098
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1098 1099
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1099
      1106
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1106 1107
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1107 1108
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1108 1109
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1109
      1110 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1110 1114
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1114
      1117
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      1117 1128
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1128 1132
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1132
      1133
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1133 1168
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1168
      1186 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1186 1187
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1187 1199
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1199
      1206
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1206 1207
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1207 1208
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1208 1209
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1209
      1210 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1210 1213
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1213
      1214
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky t
                    fontified nil)
      1214 1217
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      1217 1224
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1224 1225
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1225 1229
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1229
      1230
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1230 1265
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1265
      1283 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1283 1284
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1284 1296
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1296
      1317
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1317 1318
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1318 1319
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1319 1320
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1320
      1321 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1321 1325
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1325
      1329
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      1329 1339
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1339 1343
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1343
      1344
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1344 1397
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1397
      1398
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1398 1399
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      1399 1412
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1412
      1413
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      1413 1436
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1436
      1437
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      1437 1457
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1457
      1458
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1458 1528
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1528
      1538
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1538 1581
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1581
      1592
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1592 1593
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified nil)
      1593 1594
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-builtin-face fontified nil)
      1594 1595
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified nil)
      1595 1596
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified nil)
      1596 1602
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1602
      1603
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1603 1606
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1606
      1608 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1608 1611
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      1611 1617
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1617 1619
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1619
      1620
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1620 1661
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1661
      1662
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1662 1665
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1665
      1666
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1666 1706
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1706
      1707
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1707 1710
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1710
      1711
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1711 1754
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1754
      1755
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1755 1756
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1756
      1759 (yank-handler (evil-yank-line-handler nil t) fontified nil)
      1759 1762
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified nil)
      1762 1774
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified nil)
      1774 1776
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1776
      1777
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified nil)
      1777 1797
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified nil)
      1797 1805
      (yank-handler (evil-yank-line-handler nil t) fontified nil) 1805
      1809 (yank-handler (evil-yank-line-handler nil t) fontified t)
      1809 1810
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1810 1836
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1836
      1837
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1837 1841
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1841
      1850
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1850 1851
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1851 1872
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1872
      1873
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1873 1878
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1878
      1879
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1879 1905
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1905
      1906
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1906 1910
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1910
      1919
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1919 1920
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1920 1957
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1957
      1958
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1958 1963
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1963
      1964
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      1964 1990
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1990
      1991
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      1991 1995
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1995
      2004
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2004 2005
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2005 2020
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2020
      2021
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2021 2026
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2026
      2027
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2027 2053
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2053
      2054
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2054 2058
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2058
      2067
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2067 2068
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2068 2083
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2083
      2084
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2084 2085
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2085 2086
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2086 2088
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2088
      2092
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2092 2099
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2099 2102
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2102 2159
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2159 2162
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2162 2231
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2231 2232
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2232 2254
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2254
      2267
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2267 2268
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2268
      2282
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      2282 2284
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2284
      2285
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2285 2287
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2287
      2290
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2290 2321
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2321 2322
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2322 2352
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2352
      2353
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2353 2363
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2363 2377
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2377
      2378
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2378 2379
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2379 2381
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2381
      2384
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2384 2431
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2431 2432
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2432 2469
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2469
      2470
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2470 2478
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2478 2492
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2492
      2493
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2493 2494
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2494 2496
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2496
      2500
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2500 2511
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2511 2512
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2512 2530
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2530 2531
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2531
      2533 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2533 2534
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2534 2550
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2550 2565
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2565
      2610
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2610 2611
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2611 2612
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2612 2616
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2616
      2617
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2617 2621
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2621 2645
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2645
      2653
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2653 2654
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2654 2658
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2658
      2661
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2661 2710
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2710 2716
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2716 2718
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2718
      2719
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2719 2739
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2739 2759
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2759
      2760
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2760 2780
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2780
      2781 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2781 2782
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2782
      2783 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2783 2784
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2784
      2789 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2789 2798
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      2798 2815
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2815
      2816
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-variable-name-face fontified t)
      2816 2817
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2817 2818
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2818 2819
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2819
      2820
      (yank-handler (evil-yank-line-handler nil t) rear-nonsticky t
                    fontified t)
      2820 2824
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-delimiter-face fontified t)
      2824 2828
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2828 2829
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-comment-face fontified t)
      2829 2830
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2830 2848
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2848 2849
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2849
      2851 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2851 2852
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2852 2868
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2868 2877
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2877
      2878
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      2878 2879
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2879 2880
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2880
      2881 (yank-handler (evil-yank-line-handler nil t) fontified t)
      2881 2882
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2882 2889
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      2889 2891
      (yank-handler (evil-yank-line-handler nil t) fontified t) 2891
      2904
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-constant-face fontified t)
      2904 2905
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-type-face fontified t)
      2905 2906
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
