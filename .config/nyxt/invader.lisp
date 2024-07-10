(defun load-assets-directory (directory read-function)
  (mapcar (lambda (i)
            (setf (gethash (file-namestring i) *static-data*)
                  (funcall read-function i)))
          (uiop:directory-files directory)))

(load-assets-directory
 (asdf:system-relative-pathname :invader "")
 #'alexandria:read-file-into-string)

(defvar invader-theme
  (make-instance 'theme:theme
                 :dark-p t
                 :background-color- "#1B1B1D"       ; Very dark gray
                 :background-color "#1E1E20"        ; Slightly lighter dark gray
                 :background-color+ "#252529"       ; Even lighter dark gray
                 :on-background-color "#E6E6E6"     ; Light gray for text

                 :primary-color- "#3A3A3C"          ; Dark gray
                 :primary-color "#4A4A4D"           ; Medium dark gray
                 :primary-color+ "#5A5A5D"          ; Lighter dark gray
                 :on-primary-color "#F0F0F0"        ; Very light gray for text

                 :secondary-color- "#3A3A3C"        ; Same as primary
                 :secondary-color "#4A4A4D"         ; Same as primary
                 :secondary-color+ "#5A5A5D"        ; Same as primary
                 :on-secondary-color "#E6E6E6"      ; Same as background text

                 :action-color- "#B03030"           ; Dark red
                 :action-color "#C04040"            ; Medium dark red
                 :action-color+ "#D05050"           ; Lighter dark red
                 :on-action-color "#FFFFFF"         ; White for text

                 :success-color- "#4CAF50"          ; Green
                 :success-color "#66BB6A"           ; Medium green
                 :success-color+ "#81C784"          ; Light green
                 :on-success-color "#0C0C0D"        ; Very dark for text

                 :highlight-color- "#FF4081"        ; Pinkish red
                 :highlight-color "#FF6090"         ; Lighter pinkish red
                 :highlight-color+ "#FF80AB"        ; Even lighter pinkish red
                 :on-highlight-color "#0C0C0D"      ; Very dark for text

                 :warning-color- "#FFC107"          ; Amber
                 :warning-color "#FFD54F"           ; Lighter amber
                 :warning-color+ "#FFE082"          ; Even lighter amber
                 :on-warning-color "#0C0C0D"        ; Very dark for text

                 :codeblock-color- "#282A36"        ; Dark gray for code blocks
                 :codeblock-color "#373844"         ; Slightly lighter dark gray for code blocks
                 :codeblock-color+ "#44475A"        ; Even lighter dark gray for code blocks
                 :on-codeblock-color "#F8F8F2"))    ; Light gray for text in code blocks

(define-configuration browser
    ((theme invader-theme)))

(define-configuration status-buffer
    ((style (str:concat %slot-value%
                        (theme:themed-css (theme *browser*))))))

(in-package :nyxt)

(define-internal-page-command-global new ()
    (buffer "*New buffer*")
  "Display a page suitable as `default-new-buffer-url'."
  (spinneret:with-html-string
    (:nstyle
      `(body
        :min-height "100vh"
        :background ,(format nil "url('data:image/svg+xml;base64,~a')"
                             (cl-base64:string-to-base64-string (gethash "tiling-frame.svg" *static-data*)))
        :background-size "cover"
        :overflow "hidden"
        :padding "0"
        :margin "0")
      `(nav
        :text-align "center")
      `(.container
        :padding-top "32px"
        :display "flex"
        :flex-direction "row"
        :justify-content "center")
      `(.button
        :background-color ,theme:secondary
        :border-color ,theme:secondary
        :color ,theme:on-secondary
        :min-width "144px")
      `(.copyright
        :position "absolute"
        :bottom "12px"
        :right "48px")
      `(.program-name
        :color ,theme:action
        :font-size "24px"
        :font-weight "bold")
      `(.main
        :margin-top "35vh"
        :display "flex"
        :flex-direction "row"
        :width "900px")
      `(.logo
        :margin-top "-35px"
        :margin-right "10px")
      `(.set-url
        :min-width "180px"
        :height "40px"
        :color ,theme:on-action
        :background-color ,theme:action
        :border "none"
        :border-width "2px"
        :border-radius "3px"
        :margin-bottom "17px")
      `(.execute-command
        :min-width "180px"
        :line-height "12px"
        :height "40px"
        :border "none"
        :background-color ,theme:primary
        :border-color ,theme:primary
        :color ,theme:on-primary)
      `(.binding
        :margin-left "12px"
        :font-weight "bold"
        :color ,theme:secondary)
      `(".tentacle svg"
        :display "inline-block"
        :height "48px"
        :padding-top "4px"
        :padding-left "10px"
        :margin-bottom "-16px"))
    (:div
     :class "container"
     (:main
      (:nav
       (:nbutton :text "Quick-Start"
         '(quick-start))
       (:a :class "button" :href (nyxt-url 'describe-bindings)
           :title "List all bindings for the current buffer."
           "Describe-Bindings")
       (:a :class "button" :href (nyxt-url 'manual)
           :title "Full documentation about Nyxt, how it works and how to configure it."
           "Manual")
       (:a :class "button" :href (nyxt-url 'common-settings)
           :title "Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level."
           "Settings"))
      (:div :class "main"
            (:div :class "logo" (:raw (gethash "squid-head.svg" *static-data*)))
            (:div
             (:div (:nbutton :class "set-url" :text "Set-URL"
                     '(set-url :prefill-current-url-p nil))
                   (:span :class "binding"
                          (format nil "(~a)" (nyxt::binding-keys 'set-url)))
                   (:span :class "tentacle"
                          (:raw (gethash "upper-tentacle.svg" *static-data*))))
             (:div (:nbutton :class "execute-command" :text "Execute-Command"
                     '(execute-command))
                   (:span :class "binding"
                          (format nil "(~a)" (nyxt::binding-keys 'execute-command)))
                   (:span :class "tentacle"
                          (:raw (gethash "lower-tentacle.svg" *static-data*)))))))
     (:p :class "copyright"
         (:span :class "program-name" "Nyxt")
         (format nil " ~a (~a)" +version+ (name *renderer*))
         (:br)
         (format nil "Atlas Engineer, 2018-~a" (time:timestamp-year (time:now)))))))

;; (define-configuration nyxt/mode/hint:hint-mode
;;   ((style (str:concat
;;            %slot-value%
;;            (theme:themed-css (theme *browser*)
;;              `(".nyxt-hint"
;;                :background-color ,(cl-colors2:print-hex theme:background :alpha 0.925)
;;                :color ,*face10*;theme:on-background
;;                :font-family "monospace,monospace"
;;                :font-size ".85rem"
;;                :padding "0px 0.3em"
;;                :border-color ,(cl-colors2:print-hex theme:primary- :alpha 0.80)
;;                :border-radius "3px"
;;                :border-width "2px"
;;                :border-style "solid"
;;                :z-index #.(1- (expt 2 31)))
;;              `(".nyxt-hint.nyxt-mark-hint"
;;                :background-color ,theme:secondary
;;                :color ,theme:on-secondary
;;                :font-weight "bold")
;;              `(".nyxt-hint.nyxt-select-hint"
;;                :background-color ,theme:action
;;                :color ,theme:on-action)
;;              `(".nyxt-hint.nyxt-match-hint"
;;                :padding "0px"
;;                :border-style "none"
;;                :opacity "0.5")
;;              `(".nyxt-element-hint"
;;                :background-color ,theme:action))))))
