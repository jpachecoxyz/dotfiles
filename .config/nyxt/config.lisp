(in-package #:nyxt-user)

;; Import Files
(nyxt::load-lisp "~/.config/nyxt/style.lisp")
(nyxt::load-lisp "~/.config/nyxt/statusline.lisp")
;; (nyxt::load-lisp "~/.config/nyxt/invader.lisp")
(nyxt::load-lisp "~/.config/nyxt/commands.lisp")
(nyxt::load-lisp "~/.config/nyxt/keybinds.lisp")

(setf (uiop/os:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1")

(defmethod customize-instance ((browser browser) &key)
  (setf (slot-value browser 'restore-session-on-startup-p) nil))

;; vi mode
(define-configuration buffer
	((default-modes
		 (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration prompt-buffer
	((default-modes (append '(vi-insert-mode) %slot-default%)))) 

;; Define M-x to run commands like in emacs
(define-configuration input-buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "M-x" 'execute-command "C-space" 'nothing)))))

;; Search engines.
(defvar *my-search-engines*
  (list
   '("g" "https://google.com/search?q=~a" "https://google.com")
   '("py" "https://docs.python.org/3/search.html?q=~a"
     "https://docs.python.org/3")
   '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")
   '("yt" "https://www.youtube.com/results?search_query=~a"))
  "List of search engines.")

(define-configuration context-buffer
  "Go through the search engines above and make-search-engine out of them."
  ((search-engines
    (append
     (mapcar (lambda (engine) (apply 'make-search-engine engine))
             *my-search-engines*)
     %slot-default%))))

(define-configuration context-buffer
	"Add a single search engine manually."
  ((search-engines
    (pushnew
     (make-instance 'search-engine :name "Reddit" :shortcut "r"
                    :search-url "https://reddit.com/search/?q=~a"
                    :fallback-url "https://reddit.com")
     %slot-value%))))

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
  "Reroute the bookmarks to the config directory."
  #p"~/.config/nyxt/bookmarks.lisp")

;; Hint alphabet
(define-configuration :hint-mode
  ((nyxt/mode/hint:hints-alphabet "AOEUIDHTNS")))

;; Show the init message
(echo "Config files successfully loaded!")
