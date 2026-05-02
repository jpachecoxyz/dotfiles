;;; jp-emacs-mu4e.el --- mu4e configuration -*- lexical-binding: t; -*-

(jp-emacs-configure
  (require 'mu4e)

;;;; Mail sync
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval (* 10 60))

;;;; Maildir
  (setq mu4e-maildir (expand-file-name "~/Mail"))

  (setq mu4e-contacts-file "~/Documents/Emacs/org/agenda/contacts.org")

  ;; Autocompletado para org-contacts file
  (setq mu4e-compose-complete-addresses nil)

;;;; Identity
  (setq user-mail-address "jpacheco@disroot.org"
        user-full-name    "Javier Pacheco")

;;;; Sending mail (msmtp)
  (setq sendmail-program "msmtp"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it)

;;;; SMTP (fallback)
  (setq smtpmail-smtp-server  "disroot.org"
        smtpmail-smtp-service 587
        smtpmail-stream-type  'starttls
        smtpmail-smtp-user    "jpacheco@disroot.org")

;;;; Composition
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  (setq message-kill-buffer-on-exit t)

;;;; Important for mbsync
  (setq mu4e-change-filenames-when-moving t)

;;;; Visual
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)

;;;; Mail folders
  (setq mu4e-maildir "/disroot"
        mu4e-drafts-folder  "/disroot/Drafts"
        mu4e-sent-folder    "/disroot/Sent"
        mu4e-refile-folder  "/disroot/Archive"
        mu4e-trash-folder   "/disroot/Trash")

;;;; Inbox query
  (setq m/mu4e-inbox-query
        "(maildir:/disroot/INBOX) AND flag:unread")

;;;; Shortcuts

  (setq mu4e-maildir-shortcuts
        '((:maildir "/disroot/INBOX"  :key ?i)
          (:maildir "/disroot/Sent"   :key ?s)
          (:maildir "/disroot/Trash"  :key ?t)
          (:maildir "/disroot/Drafts" :key ?d)))

;;;; Bookmarks

  (setq mu4e-bookmarks
        '((:name "Unread messages"
                 :query "flag:unread AND NOT flag:trashed"
                 :key ?i)

          (:name "Today's messages"
                 :query "date:today..now"
                 :key ?t)

          (:name "Last 7 days"
                 :query "date:7d..now"
                 :hide-unread t
                 :key ?w)

          (:name "Messages with images"
                 :query "mime:image/*"
                 :key ?p)))

;;;; Quick inbox
  (defun jp/mu4e-go-to-inbox ()
    (interactive)
    (mu4e-headers-search m/mu4e-inbox-query))

;;;; Keybindings
  (jp-emacs-keybind global-map
    "C-c m" #'mu4e
    "C-x m" #'mu4e-compose-new))

;;; Mu4e Alert
(jp-emacs-configure
  (jp-emacs-install mu4e-alert)

  (require 'mu4e-alert)

  ;; estilo de notificación
  (setq mu4e-alert-style 'notifications)

  ;; activar notificaciones
  (mu4e-alert-enable-notifications)

  ;; actualizar correo automáticamente
  (setq mu4e-update-interval 300) ;; 5 minutos
  (mu4e-alert-enable-mode-line-display))

;;; Org-contacts
(jp-emacs-configure
  (jp-emacs-install org-contacts)
  (require 'org-contacts)

  (setq org-contacts-files '("~/Documents/Emacs/org/agenda/contacts.org"))

  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)

  ;; Backend de autocompletado
  (setq mu4e-compose-complete-addresses-function #'mu4e--compose-complete-handler)
  (add-to-list 'completion-at-point-functions #'org-contacts-message-complete-function))

(provide 'jp-emacs-mu4e)
