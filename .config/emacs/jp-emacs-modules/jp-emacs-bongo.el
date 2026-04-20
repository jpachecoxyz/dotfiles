(jp-emacs-configure
  (jp-emacs-install bongo)
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(mpv vlc))
  (setq bongo-vlc-program-name "cvlc")

;;; Bongo playlist buffer
  (defvar jp-emacs-bongo-playlist-delimiter
    "\n******************************\n\n"
    "Delimiter for inserted items in `bongo' playlist buffers.")

  (defun jp-emacs-bongo-playlist-section ()
    (bongo-insert-comment-text
     jp-emacs-bongo-playlist-delimiter))

  (defun jp-emacs-bongo-paylist-section-next ()
    "Move to next `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-forward section nil t))
          (progn
            (goto-char (point-at-eol))
            (re-search-forward section nil t))
        (goto-char (point-max)))))

  (defun jp-emacs-bongo-paylist-section-previous ()
    "Move to previous `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (goto-char (point-at-bol))
            (re-search-backward section nil t))
        (goto-char (point-min)))))

  (defun jp-emacs-bongo-playlist-mark-section ()
    "Mark `bongo' playlist section, delimited by custom markers.
The marker is `jp-emacs-bongo-playlist-delimiter'."
    (interactive)
    (let ((section "^\\*+$"))
      (search-forward-regexp section nil t)
      (push-mark nil t)
      (forward-line -1)
      ;; REVIEW any predicate to replace this `save-excursion'?
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (search-backward-regexp section nil t)
            (forward-line 1))
        (goto-char (point-min)))
      (activate-mark)))

  (defun jp-emacs-bongo-playlist-kill-section ()
    "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`jp-emacs-bongo-playlist-kill-section'."
    (interactive)
    (jp-emacs-bongo-playlist-mark-section)
    (bongo-kill))

  (defun jp-emacs-bongo-playlist-play-random ()
    "Play random `bongo' track and determine further conditions."
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode 1)
          (bongo-recenter)))))

  (defun jp-emacs-bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (if (eq bongo-next-action 'bongo-play-random-or-stop)
        (bongo-progressive-playback-mode)
      (bongo-random-playback-mode)))

  (defun jp-emacs-bongo-playlist-reset ()
    "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-reset-playlist)))

  (defun jp-emacs-bongo-playlist-terminate ()
    "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-erase-buffer)))

  (defun jp-emacs-bongo-playlist-insert-playlist-file ()
    "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`jp-emacs-bongo-play-random'.

The available options at the completion prompt point to files
that hold filesystem paths of media items.  Think of them as
'directories of directories' that mix manually selected media
items.

Also see `jp-emacs-bongo-dired-make-playlist-file'."
    (interactive)
    (let* ((path "~/Music/playlists/")
           (dotless directory-files-no-dot-files-regexp)
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files path nil dotless)))
           (choice (completing-read "Insert playlist: " playlists nil t)))
      (if (bongo-playlist-buffer-p)
          (progn
            (save-excursion
              (goto-char (point-max))
              (bongo-insert-playlist-contents
               (format "%s%s" path choice))
              (jp-emacs-bongo-playlist-section))
            (jp-emacs-bongo-playlist-play-random))
        (user-error "Not in a `bongo' playlist buffer"))))

;;; Bongo + Dired (bongo library buffer)
  (defmacro jp-emacs-bongo-dired-library (name doc val)
    "Create `bongo' library function NAME with DOC and VAL."
    `(defun ,name ()
       ,doc
       (when (string-match-p "\\`~/Music/" default-directory)
         (bongo-dired-library-mode ,val))))

  (jp-emacs-bongo-dired-library
   jp-emacs-bongo-dired-library-enable
   "Set `bongo-dired-library-mode' when accessing ~/Music.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`jp-emacs-bongo-dired-library-disable'."
   1)

  ;; NOTE `jp-emacs-bongo-dired-library-enable' does not get reactivated
  ;; upon exiting `wdired-mode'.
  ;;
  ;; TODO reactivate bongo dired library upon wdired exit
  (jp-emacs-bongo-dired-library
   jp-emacs-bongo-dired-library-disable
   "Unset `bongo-dired-library-mode' when accessing ~/Music.
This should be added `wdired-mode-hook'.  For more, refer to
`jp-emacs-bongo-dired-library-enable'."
   -1)

  (defun jp-emacs-bongo-dired-insert-files ()
    "Add files in a `dired' buffer to the `bongo' playlist."
    (let ((media (dired-get-marked-files)))
      (with-current-buffer (bongo-playlist-buffer)
        (goto-char (point-max))
        (mapc 'bongo-insert-file media)
        (jp-emacs-bongo-playlist-section))
      (with-current-buffer (bongo-library-buffer)
        (dired-next-line 1))))

  (defun jp-emacs-bongo-dired-insert ()
    "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `jp-emacs-bongo-dired-insert-files' as well as
`jp-emacs-bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `jp-emacs-bongo-dired-library')."
    (interactive)
    (when (bongo-library-buffer-p)
      (unless (bongo-playlist-buffer-p)
        (bongo-playlist-buffer))
      (jp-emacs-bongo-dired-insert-files)
      (jp-emacs-bongo-playlist-play-random)))

  (defun jp-emacs-bongo-dired-make-playlist-file ()
    "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `jp-emacs-bongo-playlist-insert-playlist-file'."
    (interactive)
    (let* ((dotless directory-files-no-dot-files-regexp)
           (pldir "~/Music/playlists")
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files pldir nil dotless)))
           (plname (completing-read "Select playlist: " playlists nil nil))
           (plfile (format "%s/%s" pldir plname))
           (media-paths
            (if (derived-mode-p 'dired-mode)
                ;; TODO more efficient way to do ensure newline ending?
                ;;
                ;; The issue is that we need to have a newline at the
                ;; end of the file, so that when we append again we
                ;; start on an empty line.
                (concat
                 (mapconcat #'identity
                            (dired-get-marked-files)
                            "\n")
                 "\n")
              (user-error "Not in a `dired' buffer"))))
      ;; The following `when' just checks for an empty string.  If we
      ;; wanted to make this more robust we should also check for names
      ;; that contain only spaces and/or invalid characters…  This is
      ;; good enough for me.
      (when (string-empty-p plname)
        (user-error "No playlist file has been specified"))
      (unless (file-directory-p pldir)
        (make-directory pldir))
      (unless (and (file-exists-p plfile)
                   (file-readable-p plfile)
                   (not (file-directory-p plfile)))
        (make-empty-file plfile))
      (append-to-file media-paths nil plfile)
      (with-current-buffer (find-file-noselect plfile)
        (delete-duplicate-lines (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer)
        (kill-buffer))))

(jp-emacs-configure
  (jp-emacs-install bongo)

  ;; Hooks
  (add-hook 'dired-mode-hook #'jp-emacs-bongo-dired-library-enable)
  (add-hook 'wdired-mode-hook #'jp-emacs-bongo-dired-library-disable)

  ;; Global keys
  (jp-emacs-keybind global-map
    "<C-XF86AudioPlay>" #'bongo-pause/resume
    "<C-XF86AudioNext>" #'bongo-next
    "<C-XF86AudioPrev>" #'bongo-previous
    "<M-XF86AudioPlay>" #'bongo-show
    "<S-XF86AudioNext>" #'bongo-seek-forward-10
    "<S-XF86AudioPrev>" #'bongo-seek-backward-10)

  (with-eval-after-load 'bongo

    ;; Playlist mode
    (jp-emacs-keybind bongo-playlist-mode-map
      "n" #'bongo-next-object
      "p" #'bongo-previous-object
      "M-n" #'jp-emacs-bongo-paylist-section-next
      "M-p" #'jp-emacs-bongo-paylist-section-previous
      "M-h" #'jp-emacs-bongo-playlist-mark-section
      "M-d" #'jp-emacs-bongo-playlist-kill-section
      "g" #'jp-emacs-bongo-playlist-reset
      "D" #'jp-emacs-bongo-playlist-terminate
      "r" #'jp-emacs-bongo-playlist-random-toggle
      "R" #'bongo-rename-line
      "j" #'bongo-dired-line
      "J" #'dired-jump
      "i" #'jp-emacs-bongo-playlist-insert-playlist-file
      "I" #'bongo-insert-special)

    ;; Dired library mode
    (jp-emacs-keybind bongo-dired-library-mode-map
      "<C-return>" #'jp-emacs-bongo-dired-insert
      "C-c SPC" #'jp-emacs-bongo-dired-insert
      "C-c +" #'jp-emacs-bongo-dired-make-playlist-file))))

(provide 'jp-emacs-bongo)
