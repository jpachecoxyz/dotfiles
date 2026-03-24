;;; The best protesilaos package ever
;; Denote Package

(jp-emacs-configure
  (jp-emacs-install denote)


(jp-emacs-configure

  ;; Hooks
  (jp-emacs-hook dired-mode-hook (denote-dired-mode)))

;; Keybinds
(jp-emacs-keybind global-map
  "C-c n n" #'denote-create-note
  "C-c n l" #'denote-link-or-create
  "C-c n L" #'denote-add-links
  "C-c n b" #'denote-link-backlinks
  "C-c n f" #'jp:denote-dired-open
  "C-c n r" #'denote-rename-file
  "C-c n R" #'denote-rename-file-using-front-matter
  "C-c n q c" #'denote-query-contents-link
  "C-c n q f" #'denote-query-filenames-link
  "C-c n i i" #'denote-insert-image)

(jp-emacs-keybind dired-mode-map
  "C-c C-d C-i" #'denote-dired-link-marked-notes
  "C-c C-d C-r" #'denote-dired-rename-files
  "C-c C-d C-f" #'denote-dired-filter
  "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
  "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

(setq denote-directory (expand-file-name "~/Documents/Emacs/notes"))
(setq denote-known-keywords '("estudio" "trabajo" "emacs" "linux"))
(setq denote-title-history nil)
(setq denote-sort-keywords nil)
(setq denote-files-matching-regexp-history nil)
(setq denote-history-completion-in-prompts nil)
(setq denote-infer-keywords t)

(setq denote-org-front-matter
      "# -*- jinx-languages: \"es_MX\"; -*-\n#+title: %s\n#+date: %s\n#+filetags: %s\n#+identifier: %s\n#+author: Ing. Javier Pacheco\n#+startup: showall\n\n")

(setq denote-query-links-display-buffer-action
      '((display-buffer-same-window)))

(setq denote-link--prepare-links-format "%s\n"))

(provide 'jp-emacs-denote)
