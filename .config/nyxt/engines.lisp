(in-package #:nyxt-user)

;; Helper
(defun set-engine (key site query)
  (list key (concatenate 'string site query) site))

;; --------------------------- Public instances -----------------------------
;;
;; SearXNG:   https://searx.space/
;; Nitter:    https://github.com/zedeus/nitter/wiki/Instances
;; Invidious: https://docs.invidious.io/instances
;; Libreddit: https://github.com/libreddit/libreddit-instances/blob/master/instances.md
;; Proxitok:  https://github.com/pablouser1/ProxiTok/wiki/Public-instances
;; Rimgo:     https://codeberg.org/video-prize-ranch/rimgo#instances
;; Tent:      https://codeberg.org/sun/Tent#Instances
;; Lingva:    https://github.com/thedaviddelta/lingva-translate#Instances
;;
;; --------------------------------------------------------------------------

(defvar *my-search-engines*
  (list

   ;; -----------------------------------------------------------------------
   ;; Social networks
   ;; -----------------------------------------------------------------------

   (set-engine "lem" "https://lemmy.ml/" "search/q/~a/type/All/sort/TopAll/listing_type/All/community_id/0/creator_id/0/page/1")
   (set-engine "ntt" "nitter.privacydev.net" "search?f=tweets&q=~a")
   (set-engine "rdd" "https://libreddit.northboot.xyz/" "r/all/search?q=~a")
   (set-engine "tkt" "https://proxitok.pabloferreiro.es/" "@~a")

   ;; -----------------------------------------------------------------------
   ;; Video
   ;; -----------------------------------------------------------------------

   (set-engine "yt" "https://invidious.projectsegfau.lt/" "search?q=~a")
   (set-engine "ods" "https://odysee.com/" "$/search?q=~a")
   (set-engine "twt" "https://www.twitch.tv/" "search?term=~a")

   ;; -----------------------------------------------------------------------
   ;; Wiki
   ;; -----------------------------------------------------------------------

   (set-engine "wen" "https://en.wikipedia.org/" "w/index.php?search=~a")
   (set-engine "wes" "https://es.wikipedia.org/" "w/index.php?search=~a")
   (set-engine "pwen" "https://en.prolewiki.org/" "index.php?search=~a&title=Special%3ASearch&go=Go")
   (set-engine "pwes" "https://es.prolewiki.org/" "index.php?title=Especial:Buscar&search=~a&go=Go")
   (set-engine "wlss" "https://wikiless.pufe.org/" "w/index.php?search=lisp&title=Special%3ASearch&fulltext=Search")
   (set-engine "libp" "https://librepedia.miraheze.org/" "w/index.php?search=~a")

   ;; -----------------------------------------------------------------------
   ;; Maps
   ;; -----------------------------------------------------------------------

   (set-engine "osm" "https://www.openstreetmap.org/" "search?query=~a")

   ;; -----------------------------------------------------------------------
   ;; Movies, series, etc
   ;; -----------------------------------------------------------------------

   (set-engine "mdb" "https://libremdb.pussthecat.org/" "find?q=~a")

   ;; -----------------------------------------------------------------------
   ;; Music
   ;; -----------------------------------------------------------------------

   (set-engine "tnt" "https://tent.sny.sh/" "search.php?query=~a")

   ;; -----------------------------------------------------------------------
   ;; Translation
   ;; -----------------------------------------------------------------------

   (set-engine "en" "https://lingva.garudalinux.org/" "es/en/~a")
   (set-engine "es" "https://lingva.garudalinux.org/" "en/es/~a")
   (set-engine "enes" "https://www.wordreference.com/" "enes/~a")
   (set-engine "esen" "https://www.wordreference.com/" "esen/~a")

   ;; -----------------------------------------------------------------------
   ;; Code
   ;; -----------------------------------------------------------------------

   ;; Nyxt
   (set-engine "nxtf" "https://discourse.atlas.engineer/" "search?q=~a")

   ;; Emacs
   (set-engine "ddoom" "https://discourse.doomemacs.org/" "search?q=~a")
   (set-engine "mlp" "https://melpa.org/" "#/?q=~a")

   ;; Arch Linux
   (set-engine "aur" "https://aur.archlinux.org/" "packages/?O=0&K=~a")
   (set-engine "archpkg" "https://archlinux.org/" "packages/?sort=&q=~a")
   (set-engine "warch" "https://wiki.archlinux.org/" "?search=~a")

   ;; Libraries
   (set-engine "pypi" "https://pypi.org/ ""search/?q=~a")

   ;; Docs
   (set-engine "py" "https://docs.python.org/" "3/search.html?q=~a")
   (set-engine "rd" "https://doc.rust-lang.org/" "std/index.html?search=~a")
   (set-engine "drs" "https://docs.rs/" "releases/search?query=~a")
   (set-engine "cpp" "https://en.cppreference.com/" "mwiki/index.php?search=~a")
   (set-engine "jdk" "https://docs.oracle.com/" "search/?q=~a&pg=1&size=10&library=en%2Fjava%2Fjavase%2F17&book=DOCS&lang=en")
   (set-engine "js" "https://developer.mozilla.org/" "en-US/search?q=~a")
   (set-engine "clj" "https://clojuredocs.org/" "search?q=~a")
   (set-engine "git" "https://git-scm.com/" "search/results?search=~a")
   (set-engine "qd" "https://quickdocs.org/" "-/search?q=~a")

   ;; -----------------------------------------------------------------------
   ;; General purpose engines
   ;; -----------------------------------------------------------------------

   (set-engine "wb" "https://wiby.me/" "?q=~a")
   (set-engine "dd" "https://html.duckduckgo.com/" "html/?q=~a")
   (set-engine "mg" "https://search.marginalia.nu/" "search?query=~a")
   (set-engine "gg" "https://www.google.com/search?q~a")
   (set-engine "GG" "https://4get.ca/" "web?s=~a")
   (set-engine "gnv" "https://gopher.emacs.ch/" "search?q=~a&source=all")
   (set-engine "sx" "https://search.atlas.engineer/searxng/" "search?q=~a") ; DEFAULT

   ;; -----------------------------------------------------------------------
   ;; End
   ))

(define-configuration buffer
  ((search-engines
    (append %slot-default%
            (mapcar (lambda (engine) (apply 'make-search-engine engine))
                    *my-search-engines*)))))

