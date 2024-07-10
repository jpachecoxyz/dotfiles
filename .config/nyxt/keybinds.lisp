(in-package #:nyxt-user)

(define-configuration :document-mode
  "Add basic keybindings."
  ((keyscheme-map
    (keymaps:define-keyscheme-map
        "custom" (list :import %slot-value%)
      nyxt/keyscheme:vi-normal
      (list "C-c e V" 'mpv-video-current-page
            "C-c e v" 'mpv-video-hint-url
            "C-c e A" 'mpv-audio-current-page
            "C-c e a" 'mpv-audio-hint-url
            "C-c e u" 'nyxt/mode/password:copy-username
            "C-c e p" 'nyxt/mode/password:copy-password
            "C-c e o" 'nyxt/mode/visual:visual-mode)))))
