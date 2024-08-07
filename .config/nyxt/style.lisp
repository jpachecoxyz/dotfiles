(in-package #:nyxt-user)

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theming

;;; Based on:
;;; https://github.com/Anomalocaridid/dotfiles

(defun make-important (property)
  (str:concat property " !important"))

;;; set color palette
(defmacro define-palette (&rest colors)
  "Helper macro to set global variables for `theme' colors."
  `(progn
     ,@(loop for (name hex)
               in colors
             collect `(defparameter ,name ,hex "Color used for `theme'."))))

(define-palette
  (*bg*                "#1d2021") ; Very dark gray
  (*fg*                "#ebdbb2") ; Light beige
  (*seperator*         "#3c3836") ; Dark gray-brown
  (*cursor*            "#d65d0e") ; Warm orange
  (*face1*             "#fb4934") ; Soft red
  (*face2*             "#b8bb26") ; Olive green
  (*face3*             "#fe8019") ; Orange
  (*face4*             "#d3869b") ; Pink
  (*face5*             "#83a598") ; Soft blue
  (*face6*             "#8ec07c") ; Light green
  (*face7*             "#d65d0e") ; Dark orange
  (*face8*             "#b16286") ; Dark pink
  (*face9*             "#7c6f64") ; Brown
  (*face10*            "#83a598") ; Soft blue
  (*bg1*               "#282828") ; Slightly lighter dark background
  (*m1*                "#3c3836") ; Dark gray-brown
  (*m2*                "#d65d0e") ; Warm orange
  (*m3*                "#fb4934") ; Soft red
  (*m4*                "#9d0006") ; Dark red
  (*m5*                "#6c6c6c") ; Medium gray
  (*m6*                "#282828") ; Very dark gray (alternative bg)
  (*m7*                "#fb4934") ; Bright red
  (*m8*                "#3c3836") ; Dark gray-brown
  (*diff1*             "#1c1c1c") ; Darker gray for diffs
  (*ml-inactive-face*  "#7c6f64") ; Medium brown for inactive
  (*ml-active-face*    "#ebdbb2") ; Light beige for active
  (*comment-face*      "#7c6f64") ; Medium brown for comments
  (*line-number-face*  "#4e4e4e") ; Dark gray for line numbers
  (*warning-bg-face*   "#3c3836") ; Dark gray-brown for warnings
  (*fullWhite*         "#ffffff") ; White
  (*fullBlack*         "#000000") ; Black
  (*highlighter*       "#d65d0e") ; Warm orange for highlight
  (*hl-line-highlight* "#2e2e2e") ; Darker background for line highlight

  ;; Additional variables
  (*dark-blue*         "#282c34") ; Very dark blue-gray
  (*semi-dark-blue*    "#32363e") ; Dark blue-gray
  (*blue*              "#458588") ; Soft blue
  (*light-blue*        "#83a598") ; Light blue
  (*trans-blue*        "#7c6f64") ; Medium brown-blue
  (*cyan*              "#8ec07c") ; Light greenish-blue
  (*pink*              "#d3869b") ; Light pink
  (*purple*            "#b16286") ; Dark purple
  (*trans-purple*      "#6c6c6c") ; Medium gray (for purple tones)
  (*red*               "#fb4934") ; Soft red
  (*orange*            "#fe8019") ; Orange
  (*white*             "#ebdbb2") ; Light beige
  (*yellow*            "#fabd2f") ; Warm yellow
  (*green*             "#b8bb26") ; Olive green
  (*font-exotica*      "IBM Plex Mono"))

(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :background-color *bg*
                         :on-background-color *face10*
                         :primary-color *face10*
                         :on-primary-color *fullWhite*
                         :secondary-color *face4*
                         :on-secondary-color *face5*
                         :accent-color *m5*
                         :on-accent-color *highlighter*
                         :font-family *font-exotica*
                         :codeblock-color- "#44355a"
                         :codeblock-color *seperator*
                         :codeblock-color+ "#221a2d"
                         :text-color "#ededed"))))

(define-configuration window
  ((message-buffer-style (str:concat
                          %slot-value%
                          (theme:themed-css (theme *browser*)
                            `(body
                              :background-color ,*dark-blue*
                              :color ,theme:on-background
                              :font-family ,theme:font-family))))))

(define-configuration prompt-buffer
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `("#prompt, #prompt-extra, #prompt-area"
               :background-color ,theme:accent
               :color ,theme:on-background)
             `(button
               :border-radius "4px")
             `("button[title=vi-normal-mode], button[title=emacs-insert-mode]:hover"
               :background-color ,theme:accent
               :border-color ,theme:accent
               :color ,theme:on-background)
             `("button[title=vi-insert-mode], button[title=emacs-normal-mode]:hover"
               :background-color ,theme:primary
               :border-color ,theme:primary
               :color ,theme:on-primary)
             `(".source-name"
               :color ,theme:on-primary
               :font-weight "bold")
             `("suggestions"
               :color ,*pink*)
             `(".source-content th"
               :background-color ,theme:background
               :border "1px solid"
               :border-color ,theme:primary
               :color ,theme:on-background
               )
             `(".source-content tr:hover"
               :color ,theme:on-background
               :font-weight "bold"
               :background-color ,theme:secondary)
             `("#selection"
               :background-color ,*trans-purple*
               :color ,theme:on-background
               :font-weight "bold")
             `(".marked"
               :background-color ,*trans-blue*
               :color ,theme:on-primary))))))

(define-configuration web-buffer
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `(*
               :font-family ,theme:font-family)
             `("a:hover"
               :color ,theme:on-accent)
             `("a:active"
               :color ,theme:on-secondary)
             `(".button, .button.set-url, .button.execute-command"
               :background-color ,(make-important theme:background) ; important necessary for gopher search buttons
               :border "1px solid"
               :border-color ,(make-important theme:primary)
               :color ,(make-important theme:on-background))
             `(".button.set-url, .button.execute-command"
               :color ,(make-important theme:primary))
             `(".button:hover"
               :background-color ,(make-important *line-number-face*) ; important necessary for gopher search buttons
               :border "1px solid"
               :border-color ,(make-important theme:secondary) ; important necessary for gopher search buttons
               :color ,theme:on-background
               :opacity 1)
             `(".button:active"
               :background-color ,(make-important theme:on-secondary) ; important necessary for gopher search buttons
               :border "1px solid"
               :border-color ,(make-important theme:on-secondary) ; important necessary for gopher search buttons
               :color ,theme:primary)
             `(".button:visited"
               :color ,theme:accent)
             `(".button:visited:active"
               :color ,theme:background)
             ;; necessary for headings on nyxt::dashboard
             `("h2, h3, h4, h5, h6"
               :color ,(make-important theme:primary))
             ;; "Browser" title text on nyxt:dashboard
             `("#subtitle"
               :color ,(make-important theme:accent)))))))

;; (define-configuration nyxt/mode/repl:repl-mode
;;   ((style (str:concat
;;             %slot-value%
;;             (theme:themed-css (theme *browser*)
;;               `(".input"
;;                 :background-color ,theme:secondary
;;                 :border "1px solid"
;;                 :border-color ,theme:primary
;;                 :color ,theme:on-background)
;;               `(".input-buffer"
;;                 :background-color ,theme:background
;;                 :color ,theme:on-background
;;                 :opacity 1)
;;               `(".input-buffer::placeholder"
;;                 :color ,theme:accent))))))

(define-configuration status-buffer
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `(*
               :font-family ,theme:font-family)
             `("#controls"
               :background-color ,theme:primary;*face10*;*face7*;*seperator*
               :color ,theme:accent;theme:primary
               )
             `("#controls button:hover"
               :color ,theme:on-primary)
             `("#controls button:active"
               :color ,theme:on-background)
             `("#url"
               :background-color ,*seperator*;theme:on-secondary
               :color ,theme:on-background)
             `(".button:hover"
               :opacity 1)
             `("#url .button:hover"
               :color ,theme:primary)
             `("#url .button:active"
               :color ,theme:on-primary)
             `("#tabs"
               :background-color ,theme:background
               :color ,theme:on-background)
             `("#tabs .button:hover"
               :color ,theme:primary)
             `("#tabs .button:active"
               :color ,theme:on-primary)
             `(".tab"
               :background-color ,theme:accent)
             `(".tab .button:hover"
               :color ,theme:primary)
             `(".tab .button:active"
               :color ,theme:on-primary)
             `("#modes"
               :background-color ,theme:accent
               :color ,theme:on-background)
             `("#modes .button:hover"
               :color ,theme:on-background)
             `("#modes .button:active"
               :color ,theme:background))))))

(define-configuration nyxt/mode/hint:hint-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `(".nyxt-hint"
               :background-color ,(cl-colors2:print-hex theme:background :alpha 0.925)
               :color ,*face10*;theme:on-background
               :font-family "monospace,monospace"
               :font-size ".85rem"
               :padding "0px 0.3em"
               :border-color ,(cl-colors2:print-hex theme:primary- :alpha 0.80)
               :border-radius "3px"
               :border-width "2px"
               :border-style "solid"
               :z-index #.(1- (expt 2 31)))
             `(".nyxt-hint.nyxt-mark-hint"
               :background-color ,theme:secondary
               :color ,theme:on-secondary
               :font-weight "bold")
             `(".nyxt-hint.nyxt-select-hint"
               :background-color ,theme:action
               :color ,theme:on-action)
             `(".nyxt-hint.nyxt-match-hint"
               :padding "0px"
               :border-style "none"
               :opacity "0.5")
             `(".nyxt-element-hint"
               :background-color ,theme:action))))))

(define-configuration nyxt/mode/bookmark:bookmark-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `(button
               :background-color ,theme:secondary
               :color ,theme:on-secondary))))))

(define-configuration nyxt/mode/search-buffer:search-buffer-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `("span[nyxt-search-mark]"
               :background-color ,(make-important *face7*)
               :color ,(make-important theme:on-primary))
             `("span[nyxt-search-mark].nyxt-current-search-mark"
               :background-color ,(make-important *face2*)
               :color ,(make-important theme:on-accent)))))))

(define-configuration nyxt/mode/small-web:small-web-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             `(pre
               :background-color ,theme:background)
             `(.search
               :background-color ,theme:action))))))
