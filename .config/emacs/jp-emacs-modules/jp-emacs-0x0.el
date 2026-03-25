;;; jp-emacs-0x0.el --- Upload to 0x0.st -*- lexical-binding: t; -*-

(jp-emacs-configure

  (defun jp/0x0-upload-text ()
    "Upload region or buffer contents to https://0x0.st."
    (interactive)
    (let* ((contents (if (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))
                       (buffer-string)))
           (temp-file (make-temp-file "0x0" nil ".txt" contents)))
      (message "Sending %s to 0x0.st..." temp-file)
      (let ((url (string-trim-right
                  (shell-command-to-string
                   (format "curl -s -F'file=@%s' https://0x0.st" temp-file)))))
        (message "The URL is %s" url)
        (kill-new url)
        (delete-file temp-file))))

  (defun jp/0x0-upload-file (file-path)
    "Upload FILE-PATH to https://0x0.st."
    (interactive "fSelect a file to upload: ")
    (message "Sending %s to 0x0.st..." file-path)
    (let ((url (string-trim-right
                (shell-command-to-string
                 (format "curl -s -F'file=@%s' https://0x0.st"
                         (expand-file-name file-path))))))
      (message "The URL is %s" url)
      (kill-new url))))

(provide 'jp-emacs-0x0)
