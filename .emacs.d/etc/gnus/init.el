;;; GENERAL ;;;
(load-library "smtpmail")
(load-library "nnimap")

(setq nnml-directory "~/.local/share/mail/Mail"
      nnfolder-directory "~/local/share/mail/Mail/Archives"
      message-directory "~/local/share/mail/Mail/"
      gnus-directory "~/.local/share/mail/Mail/News/"
	  user-full-name "Javier Pacheco"
	  user-mail-address "jpacheco@cock.li"
      nndraft-directory "~/.local/share/mail/Mail/Drafts/"
      message-auto-save-directory "~/.local/share/Mail/Autosave"
      gnus-ignored-newsgroups  ""
      gnus-summary-mark-below 0
      gnus-permanently-visible-groups ".*")

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)

;;; COCK MAIL ;;;
(setq gnus-select-method '(nnimap "mail.cock.li"
								  (nnimap-address "mail.cock.li")
								  (nnimap-server-port 993)
								  (auth-source "~/.authinfo.gpg")
								  (nnir-search-engine imap)
								  (nnimap-stream ssl)))

(setq smtpmail-starttls-credentials '(("mail.cock.li" 587 nil nil))
      smtpmail-smtp-server "mail.cock.li"
      smtpmail-default-smtp-server "mail.cock.li"
	  smtpmail-stream-type 'starttls
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587)

(setq gnus-parameters
      '((".*" (gcc-self . "Sent")))
      gnus-message-archive-group
      '((".*" "nnimap+Mail:Sent"))
      gnus-outgoing-message-group "Sent")
