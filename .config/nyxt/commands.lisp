(in-package #:nyxt-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mpv for video/audio (include livestreams)

(defun mpv-player (url &optional (opt "--fs"))
  (uiop:launch-program (list "mpv" opt url)))

(define-command mpv-video-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (mpv-player (render-url (url buffer))))

(define-command mpv-audio-current-page (&optional (buffer (current-buffer)))
  "Play audio in the currently open buffer."
  (mpv-player (render-url (url buffer)) "--profile=only-audio"))

(define-command mpv-video-hint-url ()
  "Show a set of element hints, and open the video of the user inputted one."
  (nyxt/mode/hint:query-hints
   "Open the video"
   (lambda (result)
     (mpv-player (render-url (url (first result)))))))

(define-command mpv-audio-hint-url ()
  "Show a set of element hints, and open the audio of the user inputted one."
  (nyxt/mode/hint:query-hints
   "Open the audio"
   (lambda (result)
     (mpv-player (render-url (url (first result))) "--profile=only-audio"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mpv for video/audio (include livestreams)

(defun streamlink (url &optional (opt "--profile=youtube"))
  (uiop:launch-program
   (list "streamlink" "--twitch-disable-ads" "-p /bin/mpv" (format nil "-a ~a" opt) url "best")))

(define-command streamlink-video-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (streamlink (render-url (url buffer))))

(define-command streamlink-audio-current-page (&optional (buffer (current-buffer)))
  "Play audio in the currently open buffer."
  (streamlink (render-url (url buffer)) "--profile=only-audio"))

(define-command streamlink-video-hint-url ()
  "Show a set of element hints, and open the video of the user inputted one."
  (nyxt/mode/hint:query-hints
   "Open the video"
   (lambda (result)
     (streamlink (render-url (url (first result)))))))

(define-command streamlink-audio-hint-url ()
  "Show a set of element hints, and open the audio of the user inputted one."
  (nyxt/mode/hint:query-hints
   "Open the audio"
   (lambda (result)
     (streamlink (render-url (url (first result))) "--profile=only-audio"))))
