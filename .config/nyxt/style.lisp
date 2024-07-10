(in-package #:nyxt-user)

(setf (uiop:getenv "GTK_THEME") ""Graphite)

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
  (*dark-blue*         "#000b1e")
  (*semi-dark-blue*    "#00005f")
  (*blue*              "#091833")
  (*light-blue*        "#133e7c")
  (*trans-blue*        "#0b2956")
  (*cyan*              "#0abdc6")
  (*pink*              "#ea00d9")
  (*purple*            "#711c91")
  (*trans-purple*      "#321959")
  (*red*               "#ff0000")
  (*orange*            "#f57800")
  (*white*             "#d7d7d5")
  (*yellow*            "#ffff00")
  (*green*             "#00ff00")
  ;; Exotica theme
  (*bg*                "#091423")
  (*fg*                "#E8F0FF")
  (*seperator*         "#132947")
  (*cursor*            "#F8F8F0")
  (*face1*             "#66D9EF")
  (*face2*             "#A6E22E")
  (*face3*             "#FF84C9")
  (*face4*             "#AE81FF")
  (*face5*             "#4577D7")
  (*face6*             "#2ee267")
  (*face7*             "#FF5996")
  (*face8*             "#60FCEC")
  (*face9*             "#344256")
  (*face10*            "#84B5FF")
  (*bg1*               "#403D3D")
  (*m1*                "#C1CAFF")
  (*m2*                "#FD971F")
  (*m3*                "#EF5939")
  (*m4*                "#960050")
  (*m5*                "#BCA3A3")
  (*m6*                "#272822")
  (*m7*                "#FF0000")
  (*m8*                "#FFCACA")
  (*diff1*             "#232526")
  (*ml-inactive-face*  "#BCBCBC")
  (*ml-active-face*    "#050302")
  (*comment-face*      "#646F84")
  (*line-number-face*  "#455770")
  (*warning-bg-face*   "#333333")
  (*fullWhite*         "#FFFFFF")
  (*fullBlack*         "#000000")
  (*highlighter*       "#E7F221")
  (*hl-line-highlight* "#182538")
  (*font-exotica*      "IBM Plex Mono"))

(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :background-color *bg*
                         :on-background-color *face1*
                         :primary-color *face10*
                         :on-primary-color *fullWhite*
                         :secondary-color *face4*
                         :on-secondary-color *face5*
                         :accent-color *m4*
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
