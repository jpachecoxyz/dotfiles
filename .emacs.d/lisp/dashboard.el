(use-package all-the-icons)

(use-package dashboard
  :hook
  (after-init . dashboard-setup-startup-hook)
  :ensure t
  :config
  (setq dashboard-set-heading-icons t
		dashboard-set-file-icons t
		dashboard-set-init-info t
		dashboard-set-navigator t)
  (setq dashboard-banner-logo-title
		(seq-random-elt  '("Automation is good, so long as you know exactly where to put the machine. – Eliyahu Goldratt"
						   "Anything that can be automated will be automated. – Shoshana Zuboff"
						   "Automation applied to an efficient operation will magnify the efficiency. – Bill Gates"
						   "The first rule of any technology used in a business is that automation applied to an efficient operation will magnify the efficiency. – Bill Gates"
						   "Automation doesn’t mean replacing humans. It means freeing them to work smarter. – Unknown"
						   "Focus on automating tasks, not roles. – Unknown"
						   "You can’t automate creativity, but you can free up time for it through automation. – Unknown"
						   "By automating processes, you create more time for innovation. – Unknown"
						   "The real power of automation lies in its ability to eliminate redundancy and error. – Unknown"
						   "Automation is the way to simplify your work, not your thinking. – Unknown"
						   "Programs must be written for people to read, and only incidentally for machines to execute. – Harold Abelson and Gerald Jay Sussman"
						   "Any fool can write code that a computer can understand. Good programmers write code that humans can understand. – Martin Fowler"
						   "Programming isn’t about what you know; it’s about what you can figure out. – Chris Pine"
						   "The best error message is the one that never shows up. – Thomas Fuchs"
						   "Give a man a program, frustrate him for a day. Teach a man to program, frustrate him for a lifetime. – Muhammad Waseem"
						   "First, solve the problem. Then, write the code. – John Johnson"
						   "Code is like humor. When you have to explain it, it’s bad. – Cory House"
						   "The only way to learn a new programming language is by writing programs in it. – Dennis Ritchie"
						   "Deleted code is debugged code. – Jeff Sickel"
						   "Programming is the art of algorithm design and the craft of debugging errant code. – Ellen Ullman")))

  ;; Define una lista de imágenes
  (setq my/dashboard-images '("~/.emacs.d/GNU.png"))
  ;; Selecciona una imagen aleatoria de la lista
  (setq dashboard-startup-banner (nth (random (length my/dashboard-images)) my/dashboard-images))) 

;; (setq dashboard-page-separator "\n\f\n")
(setq dashboard-items '((recents  . 5)
						(bookmarks . 5)
						;; (projects . 3)
						(agenda . 5)
						;; (registers . 5)
						))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; para usar los iconos de nerd font
(setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package


(setq dashboard-item-names '(("Recent Files:" . "Archivos Recientes:")
                             ("Agenda for today:" . "Para hoy agenda:")
                             ("Agenda for the coming week:" . "Agenda:")
							 ("Projects:" . "Proyectos:")))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))

(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

(use-package all-the-icons-ivy
  :init (all-the-icons-ivy-setup))

(global-set-key (kbd "<f10>") 'dashboard-open)
(setq dashboard-week-agenda t)

(provide 'dashboard)
