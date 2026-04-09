;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((97) (113 . [escape])
 (48
  . #("denote-sort-dired is an autoloaded interactive native-comp-function in\n‘denote.el’.\n\nIt can be invoked from the menu: Denote → Generate sorted and filtered\nDired listing\n\n(denote-sort-dired FILES-MATCHING-REGEXP SORT-BY-COMPONENT REVERSE\nEXCLUDE-REGEXP)\n\nProduce Dired buffer with sorted files from variable ‘denote-directory’.\nWhen called interactively, prompt for FILES-MATCHING-REGEXP and,\ndepending on the value of the user option ‘denote-sort-dired-extra-prompts’,\nalso prompt for SORT-BY-COMPONENT, REVERSE, and EXCLUDE-REGEXP.\n\n1. FILES-MATCHING-REGEXP limits the list of Denote files to\n   those matching the provided regular expression.\n\n2. SORT-BY-COMPONENT sorts the files by their file name component (one\n   among ‘denote-sort-components’).  If it is nil, sorting is performed\n   according to the user option ‘denote-sort-dired-default-sort-component’,\n   falling back to the identifier.\n\n3. REVERSE is a boolean to reverse the order when it is a non-nil value.\n   If ‘denote-sort-dired-extra-prompts’ is configured to skip this\n   prompt, then the sorting is done according to the user option\n   ‘denote-sort-dired-default-reverse-sort’, falling back to\n   nil (i.e. no reverse sort).\n\n4. EXCLUDE-REGEXP excludes the files that match the given regular\n   expression.  This is done after FILES-MATCHING-REGEXP and\n   OMIT-CURRENT have been applied.\n\nWhen called from Lisp, the arguments are a string, a symbol among\n‘denote-sort-components’, a non-nil value, and a string, respectively.\n"
      0 21 (yank-handler (evil-yank-line-handler nil t)) 21 47
      (yank-handler (evil-yank-line-handler nil t)) 47 67
      (yank-handler (evil-yank-line-handler nil t) button (t) category
                    help-type-button help-args (native-comp-function))
      67 70 (yank-handler (evil-yank-line-handler nil t)) 70 71
      (yank-handler (evil-yank-line-handler nil t)) 71 72
      (yank-handler (evil-yank-line-handler nil t)) 72 81
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-dired
                     "/home/javier/.config/emacs/elpa/denote-4.1.3.0.20260319.51307/denote.el")
                    category help-function-def-button button (t))
      81 82 (yank-handler (evil-yank-line-handler nil t)) 82 83
      (yank-handler (evil-yank-line-handler nil t)) 83 84
      (yank-handler (evil-yank-line-handler nil t)) 84 85
      (yank-handler (evil-yank-line-handler nil t)) 85 118
      (yank-handler (evil-yank-line-handler nil t)) 118 124
      (yank-handler (evil-yank-line-handler nil t) face
                    help-key-binding)
      124 127 (yank-handler (evil-yank-line-handler nil t)) 127 155
      (yank-handler (evil-yank-line-handler nil t) face
                    help-key-binding)
      155 156
      (yank-handler (evil-yank-line-handler nil t) face
                    help-key-binding)
      156 169
      (yank-handler (evil-yank-line-handler nil t) face
                    help-key-binding)
      169 170 (yank-handler (evil-yank-line-handler nil t)) 170 171
      (yank-handler (evil-yank-line-handler nil t)) 171 190
      (yank-handler (evil-yank-line-handler nil t)) 190 211
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      211 212 (yank-handler (evil-yank-line-handler nil t)) 212 229
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      229 230 (yank-handler (evil-yank-line-handler nil t)) 230 237
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      237 238 (yank-handler (evil-yank-line-handler nil t)) 238 252
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      252 253 (yank-handler (evil-yank-line-handler nil t)) 253 254
      (yank-handler (evil-yank-line-handler nil t)) 254 255
      (yank-handler (evil-yank-line-handler nil t)) 255 309
      (yank-handler (evil-yank-line-handler nil t)) 309 325
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-directory) category help-variable-button
                    button (t))
      325 366 (yank-handler (evil-yank-line-handler nil t)) 366 387
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      387 436 (yank-handler (evil-yank-line-handler nil t)) 436 467
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-dired-extra-prompts) category
                    help-variable-button button (t))
      467 486 (yank-handler (evil-yank-line-handler nil t)) 486 503
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      503 505 (yank-handler (evil-yank-line-handler nil t)) 505 512
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      512 518 (yank-handler (evil-yank-line-handler nil t)) 518 532
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      532 538 (yank-handler (evil-yank-line-handler nil t)) 538 559
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      559 650 (yank-handler (evil-yank-line-handler nil t)) 650 667
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      667 728 (yank-handler (evil-yank-line-handler nil t)) 728 750
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-components) category
                    help-symbol-button button (t))
      750 823 (yank-handler (evil-yank-line-handler nil t)) 823 863
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-dired-default-sort-component)
                    category help-variable-button button (t))
      863 905 (yank-handler (evil-yank-line-handler nil t)) 905 912
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      912 982 (yank-handler (evil-yank-line-handler nil t)) 982 1013
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-dired-extra-prompts) category
                    help-symbol-button button (t))
      1013 1111 (yank-handler (evil-yank-line-handler nil t)) 1111
      1149
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-dired-default-reverse-sort) category
                    help-variable-button button (t))
      1149 1203 (yank-handler (evil-yank-line-handler nil t)) 1203
      1217
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      1217 1301 (yank-handler (evil-yank-line-handler nil t)) 1301
      1322
      (yank-handler (evil-yank-line-handler nil t) face
                    help-argument-name)
      1322 1430 (yank-handler (evil-yank-line-handler nil t)) 1430
      1452
      (yank-handler (evil-yank-line-handler nil t) help-args
                    (denote-sort-components) category
                    help-symbol-button button (t))
      1452 1499 (yank-handler (evil-yank-line-handler nil t)) 1499
      1500 (yank-handler (evil-yank-line-handler nil t)))))
