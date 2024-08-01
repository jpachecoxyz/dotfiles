;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Sat, 20 Jan 2024 21:05:38 -0600")

(setq gnus-newsrc-alist '(("INBOX" 3 ((1 . 176)) ((unexist (1 . 76) 78 (81 . 85) (124 . 143) 145 148 (153 . 156) (159 . 164)) (seen 77 (79 . 80) (86 . 142) 144 (146 . 147) (149 . 153) (157 . 161) (165 . 176))) nil ((modseq . "303") (uidvalidity . "1658374927") (active 1 . 176) (permanent-flags %Answered %Flagged %Deleted %Seen %Draft %*))) ("Junk" 3 (1) ((unexist 1)) nil ((modseq . "4") (uidvalidity . "1658374928") (active 1 . 1) (permanent-flags %Answered %Flagged %Deleted %Seen %Draft %*))) ("Send" 3 ((1 . 23)) ((unexist) (reply 19)) nil ((modseq . "26") (uidvalidity . "1658374930") (active 1 . 23) (permanent-flags %Answered %Flagged %Deleted %Seen %Draft $Forwarded %*))) ("Trash" 3 ((1 . 115)) ((unexist (1 . 115))) nil ((modseq . "44") (uidvalidity . "1658374929") (active 1 . 115) (permanent-flags %Answered %Flagged %Deleted %Seen %Draft %*))) ("nndraft:drafts" 1 ((2 . 3)) nil (nndraft "") ((gnus-dummy (gnus-draft-mode))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/local/share/mail/Mail/archive") (nnfolder-active-file "~/local/share/mail/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))
