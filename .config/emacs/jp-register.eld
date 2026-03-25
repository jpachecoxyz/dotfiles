;; Auto-generated file: DO NOT EDIT -*- mode: emacs-lisp -*-
((114) (97) (113 . [escape escape escape escape])
 (48
  . #("(jp-emacs-configure\n\n  (defun jp/0x0-upload-text ()\n    \"Upload region or buffer contents to https://0x0.st.\"\n    (interactive)\n    (let* ((contents (if (use-region-p)\n                         (buffer-substring-no-properties\n                          (region-beginning)\n                          (region-end))\n                       (buffer-string)))\n           (temp-file (make-temp-file \"0x0\" nil \".txt\" contents)))\n      (message \"Sending %s to 0x0.st...\" temp-file)\n      (let ((url (string-trim-right\n                  (shell-command-to-string\n                   (format \"curl -s -F'file=@%s' https://0x0.st\" temp-file)))))\n        (message \"The URL is %s\" url)\n        (kill-new url)\n        (delete-file temp-file))))\n\n  (defun jp/0x0-upload-file (file-path)\n    \"Upload FILE-PATH to https://0x0.st.\"\n    (interactive \"fSelect a file to upload: \")\n    (message \"Sending %s to 0x0.st...\" file-path)\n    (let ((url (string-trim-right\n                (shell-command-to-string\n                 (format \"curl -s -F'file=@%s' https://0x0.st\"\n                         (expand-file-name file-path))))))\n      (message \"The URL is %s\" url)\n      (kill-new url))))\n"
      0 1 (yank-handler (evil-yank-line-handler nil t) fontified t) 1
      19
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      19 24 (yank-handler (evil-yank-line-handler nil t) fontified t)
      24 29
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      29 30 (yank-handler (evil-yank-line-handler nil t) fontified t)
      30 48
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      48 56 (yank-handler (evil-yank-line-handler nil t) fontified t)
      56 109
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      109 115
      (yank-handler (evil-yank-line-handler nil t) fontified t) 115
      126
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      126 133
      (yank-handler (evil-yank-line-handler nil t) fontified t) 133
      137
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      137 150
      (yank-handler (evil-yank-line-handler nil t) fontified t) 150
      152
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      152 389
      (yank-handler (evil-yank-line-handler nil t) fontified t) 389
      394
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      394 399
      (yank-handler (evil-yank-line-handler nil t) fontified t) 399
      405
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      405 433
      (yank-handler (evil-yank-line-handler nil t) fontified t) 433
      458
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      458 477
      (yank-handler (evil-yank-line-handler nil t) fontified t) 477
      480
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      480 576
      (yank-handler (evil-yank-line-handler nil t) fontified t) 576
      588
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      588 596
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-string-face)
                    fontified t)
      596 613
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      613 646
      (yank-handler (evil-yank-line-handler nil t) fontified t) 646
      661
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      661 729
      (yank-handler (evil-yank-line-handler nil t) fontified t) 729
      734
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      734 735
      (yank-handler (evil-yank-line-handler nil t) fontified t) 735
      753
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-function-name-face fontified t)
      753 770
      (yank-handler (evil-yank-line-handler nil t) fontified t) 770
      807
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-doc-face fontified t)
      807 813
      (yank-handler (evil-yank-line-handler nil t) fontified t) 813
      824
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      824 825
      (yank-handler (evil-yank-line-handler nil t) fontified t) 825
      853
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      853 868
      (yank-handler (evil-yank-line-handler nil t) fontified t) 868
      893
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      893 910
      (yank-handler (evil-yank-line-handler nil t) fontified t) 910
      913
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-keyword-face fontified t)
      913 1005
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1005
      1017
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1017 1025
      (yank-handler (evil-yank-line-handler nil t) face
                    (font-lock-constant-face font-lock-string-face)
                    fontified t)
      1025 1042
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1042 1117
      (yank-handler (evil-yank-line-handler nil t) fontified t) 1117
      1132
      (yank-handler (evil-yank-line-handler nil t) face
                    font-lock-string-face fontified t)
      1132 1162
      (yank-handler (evil-yank-line-handler nil t) fontified t))))
