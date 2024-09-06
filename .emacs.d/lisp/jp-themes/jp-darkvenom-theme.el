;; jp-darkvenom-theme.el --- Javier Pacheco custom color theme

;; Author: Javier Pacheco <javier@jpacheco.xyz>
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Embrace the elegance and sophistication of Jp-Darkvenom as it transforms your Emacs
;; environment into a realm of sublime beauty and functionality. Whether you're a
;; seasoned developer or a coding enthusiast, let Jp-Darkvenom be your guide as you
;; embark on your coding journey, illuminating the path with its captivating allure."

;;; Code:

(deftheme jp-darkvenom
  "jp-darkvenom - my Emacs personal custom theme")

(defvar jp-darkvenom-colors-alist
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
         (colors `(("jp-darkvenom-accent"   . "#41728e")
                   ("jp-darkvenom-fg"       . (if ,256color "color-248" "#c5c8c6"))
                   ("jp-darkvenom-bg"       . (if ,256color "color-235" "#000000"))
                   ("jp-darkvenom-bg-1"     . (if ,256color "color-234" "#2f2f2f"))
                   ("jp-darkvenom-bg-hl"    . (if ,256color "color-236" "#1b1b1b"))
                   ("jp-darkvenom-gutter"   . (if ,256color "color-239" "#0f0f0f"))
                   ("jp-darkvenom-mono-1"   . (if ,256color "color-248" "#292b2b"))
                   ("jp-darkvenom-mono-2"   . (if ,256color "color-244" "#3f4040"))
                   ("jp-darkvenom-mono-3"   . (if ,256color "color-240" "#5c5e5e"))
                   ("jp-darkvenom-cyan"     . "#8abeb7")				   
                   ("jp-darkvenom-blue"     . "#81a2be")
                   ("jp-darkvenom-purple"   . "#b294bb")
                   ("jp-darkvenom-green"    . "#b5bd68")
                   ("jp-darkvenom-red-1"    . "#cc6666")
                   ("jp-darkvenom-red-2"    . "#dc6666")
                   ("jp-darkvenom-orange-1" . "#de935f")
                   ("jp-darkvenom-orange-2" . "#fe935f")
                   ("jp-darkvenom-gray"     . (if ,256color "color-237" "#5a5b5a"))
                   ("jp-darkvenom-silver"   . (if ,256color "color-247" "#9DA5B4"))
                   ("jp-darkvenom-black"    . (if ,256color "color-233" "#21252B"))
                   ("jp-darkvenom-border"   . (if ,256color "color-232" "#181A1F")))))
    colors)
  "List of jp-darkvenom-theme colors.")

(defmacro jp-darkvenom-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    jp-darkvenom-colors-alist))
     ,@body))

(jp-darkvenom-with-color-variables
  (custom-theme-set-faces
   'jp-darkvenom
   
   `(default ((t (:foreground ,jp-darkvenom-blue :background ,jp-darkvenom-bg))))
   `(success ((t (:foreground ,jp-darkvenom-green))))
   `(warning ((t (:foreground ,jp-darkvenom-orange-2))))
   `(error ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))
   `(link ((t (:foreground ,jp-darkvenom-purple :underline t ))))
   `(link-visited ((t (:foreground ,jp-darkvenom-blue :underline t :weight normal))))
   `(cursor ((t (:background ,jp-darkvenom-accent))))
   `(fringe ((t (:background ,jp-darkvenom-bg))))
   `(region ((t (:background ,jp-darkvenom-gray :distant-foreground ,jp-darkvenom-mono-2))))
   `(highlight ((t (:background ,jp-darkvenom-gray :distant-foreground ,jp-darkvenom-mono-2))))
   `(hl-line ((t (:background ,jp-darkvenom-bg-hl :distant-foreground nil))))
   `(header-line ((t (:background ,jp-darkvenom-black))))
   `(vertical-border ((t (:background ,jp-darkvenom-border :foreground ,jp-darkvenom-border))))
   `(secondary-selection ((t (:background ,jp-darkvenom-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,jp-darkvenom-silver))))
   `(italic ((t (:underline nil :italic t))))
   `(org-meta-line ((t (:italic nil :foreground ,jp-darkvenom-mono-2))))
   `(tooltip ((t (:foreground ,jp-darkvenom-fg :background ,jp-darkvenom-bg-1 :inherit variable-pitch))))

   `(font-lock-builtin-face ((t (:foreground ,jp-darkvenom-cyan :italic t))))
   `(font-lock-comment-face ((t (:foreground ,jp-darkvenom-cyan :italic t))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,jp-darkvenom-blue :italic t))))
   `(font-lock-keyword-face ((t (:foreground ,jp-darkvenom-purple ))))
   `(font-lock-preprocessor-face ((t (:foreground ,jp-darkvenom-mono-2))))
   `(font-lock-string-face ((t (:foreground ,jp-darkvenom-green :italic t))))
   `(font-lock-type-face ((t (:foreground ,jp-darkvenom-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,jp-darkvenom-cyan :bold t))))
   `(font-lock-variable-name-face ((t (:foreground ,jp-darkvenom-red-1))))
   `(font-lock-warning-face ((t (:foreground ,jp-darkvenom-mono-3 :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,jp-darkvenom-cyan :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-silver :box (:color ,jp-darkvenom-border :line-width 2)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,jp-darkvenom-border :foreground ,jp-darkvenom-gray :box (:color ,jp-darkvenom-border :line-width 1)))))

   ;; window-divider
   `(window-divider ((t (:foreground ,jp-darkvenom-border))))
   `(window-divider-first-pixel ((t (:foreground ,jp-darkvenom-border))))
   `(window-divider-last-pixel ((t (:foreground ,jp-darkvenom-border))))
   
   ;; highlight-thing
   `(highlight-thing ((t :underline t)))
   `(lazy-highlight ((t :background ,jp-darkvenom-bg-hl)))


   ;; custom
   `(custom-state ((t (:foreground ,jp-darkvenom-green))))

   ;; ido
   `(ido-first-match ((t (:foreground ,jp-darkvenom-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,jp-darkvenom-blue))))
   `(ido-virtual ((t (:foreground ,jp-darkvenom-mono-3))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,jp-darkvenom-purple :background ,jp-darkvenom-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,jp-darkvenom-red-1 :background ,jp-darkvenom-bg-1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,jp-darkvenom-red-1 :background ,jp-darkvenom-gray))))
   `(company-tooltip-selection ((t (:foreground ,jp-darkvenom-fg :background ,jp-darkvenom-gray))))
   `(company-tooltip-mouse ((t (:background ,jp-darkvenom-gray))))
   `(company-tooltip-common ((t (:foreground ,jp-darkvenom-cyan :background ,jp-darkvenom-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,jp-darkvenom-cyan :background ,jp-darkvenom-gray))))
   `(company-preview ((t (:background ,jp-darkvenom-bg))))
   `(company-preview-common ((t (:foreground ,jp-darkvenom-orange-2 :background ,jp-darkvenom-bg))))
   `(company-scrollbar-fg ((t (:background ,jp-darkvenom-mono-1))))
   `(company-scrollbar-bg ((t (:background ,jp-darkvenom-bg-1))))
   `(company-template-field ((t (:inherit highlight))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,jp-darkvenom-orange-1 :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,jp-darkvenom-red-1 :style wave)))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,jp-darkvenom-red-1 :style wave)))))
   `(flymake-note ((t (:underline (:color ,jp-darkvenom-green :style wave)))))
   `(flymake-warning ((t (:underline (:color ,jp-darkvenom-orange-1 :style wave)))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color ,jp-darkvenom-red-1 :style wave)))))
   `(flycheck-info ((t (:underline (:color ,jp-darkvenom-green :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,jp-darkvenom-orange-1 :style wave)))))

   ;; compilation
   `(compilation-face ((t (:foreground ,jp-darkvenom-fg))))
   `(compilation-line-number ((t (:foreground ,jp-darkvenom-mono-2))))
   `(compilation-column-number ((t (:foreground ,jp-darkvenom-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,jp-darkvenom-bg :background ,jp-darkvenom-purple))))
   `(isearch-fail ((t (:foreground ,jp-darkvenom-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,jp-darkvenom-purple :background ,jp-darkvenom-bg-1 :underline ,jp-darkvenom-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; dired-async
   `(dired-async-failures ((t (:inherit error))))
   `(dired-async-message ((t (:inherit success))))
   `(dired-async-mode-message ((t (:foreground ,jp-darkvenom-orange-1))))

   ;; diredfl
   `(diredfl-autofile-name ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-compressed-file-name ((t (:foreground ,jp-darkvenom-red-1))))
   `(diredfl-compressed-file-suffix ((t (:foreground ,jp-darkvenom-red-1))))
   `(diredfl-date-time ((t (:foreground ,jp-darkvenom-mono-3))))
   `(diredfl-deletion ((t (:foreground ,jp-darkvenom-red-1 :background ,jp-darkvenom-bg-hl))))
   `(diredfl-deletion-file-name ((t (:foreground ,jp-darkvenom-mono-1))))
   `(diredfl-dir-heading ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-dir-name ((t (:foreground ,jp-darkvenom-purple))))
   `(diredfl-dir-priv ((t (:foreground ,jp-darkvenom-purple))))
   `(diredfl-exec-priv ((t (:foreground ,jp-darkvenom-cyan))))
   `(diredfl-executable-tag ((t (:foreground ,jp-darkvenom-cyan))))
   `(diredfl-file-name ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-file-suffix ((t (:foreground ,jp-darkvenom-blue :bold t))))
   `(diredfl-flag-mark ((t (:foreground ,jp-darkvenom-blue :bold t))))
   `(diredfl-flag-mark-line ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-ignored-file-name ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-link-priv ((t (:foreground ,jp-darkvenom-purple))))
   `(diredfl-no-priv ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-number ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-other-priv ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-rare-priv ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-read-priv ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-symlink ((t (:foreground ,jp-darkvenom-purple))))
   `(diredfl-tagged-autofile-name ((t (:foreground ,jp-darkvenom-blue))))
   `(diredfl-write-priv ((t (:foreground ,jp-darkvenom-blue))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,jp-darkvenom-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,jp-darkvenom-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,jp-darkvenom-orange-2 :weight bold))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,jp-darkvenom-green :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,jp-darkvenom-orange-1 :weight bold))))

   ;; eshell
   `(eshell-ls-archive ((t (:foreground ,jp-darkvenom-purple :weight bold))))
   `(eshell-ls-backup ((t (:foreground ,jp-darkvenom-orange-2))))
   `(eshell-ls-clutter ((t (:foreground ,jp-darkvenom-red-2 :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,jp-darkvenom-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,jp-darkvenom-green :weight bold))))
   `(eshell-ls-missing ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))
   `(eshell-ls-product ((t (:foreground ,jp-darkvenom-orange-2))))
   `(eshell-ls-special ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,jp-darkvenom-cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,jp-darkvenom-mono-1))))
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))

   ;; vterm
   `(vterm               :foreground ,jp-darkvenom-fg)
   `(vterm-color-black  (( t (:background ,jp-darkvenom-black   :foreground ,jp-darkvenom-black))))
   `(vterm-color-red    (( t (:background ,jp-darkvenom-red-1     :foreground ,jp-darkvenom-red-1))))
   `(vterm-color-green  (( t (:background ,jp-darkvenom-green   :foreground ,jp-darkvenom-green))))
   `(vterm-color-yellow (( t (:background ,jp-darkvenom-orange-1  :foreground ,jp-darkvenom-orange-1))))
   `(vterm-color-blue   (( t (:background ,jp-darkvenom-blue    :foreground ,jp-darkvenom-blue))))
   `(vterm-color-magenta(( t (:background ,jp-darkvenom-purple :foreground ,jp-darkvenom-purple))))
   `(vterm-color-cyan   (( t (:background ,jp-darkvenom-cyan    :foreground ,jp-darkvenom-cyan))))
   `(vterm-color-white  (( t (:background ,jp-darkvenom-mono-1   :foreground ,jp-darkvenom-mono-1))))

   ;; man
   `(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold ((t (:inherit font-lock-type-face :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face ((t (:inherit widget-button))))
   `(dictionary-reference-face ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; erc
   `(erc-error-face ((t (:inherit error))))
   `(erc-input-face ((t (:inherit shadow))))
   `(erc-my-nick-face ((t (:foreground ,jp-darkvenom-accent))))
   `(erc-notice-face ((t (:inherit font-lock-comment-face))))
   `(erc-timestamp-face ((t (:foreground ,jp-darkvenom-green :weight bold))))

   ;; eww
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:background ,jp-darkvenom-gray :foreground ,jp-darkvenom-fg :box (:line-width 2 :color ,jp-darkvenom-border :style released-button)))))
   `(eww-form-text ((t (:inherit widget-field :box (:line-width 1 :color ,jp-darkvenom-border)))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,jp-darkvenom-red-1))))
   `(eww-valid-certificate ((t (:foreground ,jp-darkvenom-green))))

   ;; vc-mode
   `(vc-diff-added ((t (:foreground ,jp-darkvenom-green))))
   `(vc-diff-removed ((t (:foreground ,jp-darkvenom-red-1))))
   
   ;; ediff
   `(ediff-fine-diff-Ancestor                ((t (:background "#885555"))))
   `(ediff-fine-diff-A                       ((t (:background "#885555"))))
   `(ediff-fine-diff-B                       ((t (:background "#558855"))))
   `(ediff-fine-diff-C                       ((t (:background "#555588"))))
   `(ediff-current-diff-Ancestor             ((t (:background "#663333"))))
   `(ediff-current-diff-A                    ((t (:background "#663333"))))
   `(ediff-current-diff-B                    ((t (:background "#336633"))))
   `(ediff-current-diff-C                    ((t (:background "#333366"))))
   `(ediff-even-diff-Ancestor                ((t (:background "#181a1f"))))
   `(ediff-even-diff-A                       ((t (:background "#181a1f"))))
   `(ediff-even-diff-B                       ((t (:background "#181a1f"))))
   `(ediff-even-diff-C                       ((t (:background "#181a1f"))))
   `(ediff-odd-diff-Ancestor                 ((t (:background "#181a1f"))))
   `(ediff-odd-diff-A                        ((t (:background "#181a1f"))))
   `(ediff-odd-diff-B                        ((t (:background "#181a1f"))))
   `(ediff-odd-diff-C                        ((t (:background "#181a1f"))))

   ;; magit
   `(magit-section-highlight ((t (:background ,jp-darkvenom-bg-hl))))
   `(magit-section-heading ((t (:foreground ,jp-darkvenom-orange-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,jp-darkvenom-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,jp-darkvenom-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,jp-darkvenom-orange-2 :background ,jp-darkvenom-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,jp-darkvenom-mono-2 :background ,jp-darkvenom-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,jp-darkvenom-mono-1 :background ,jp-darkvenom-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,jp-darkvenom-purple :background ,jp-darkvenom-mono-3))))
   `(magit-diff-context ((t (:foreground ,jp-darkvenom-fg))))
   `(magit-diff-context-highlight ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-fg))))
   `(magit-diffstat-added ((t (:foreground ,jp-darkvenom-green))))
   `(magit-diffstat-removed ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-process-ok ((t (:foreground ,jp-darkvenom-green))))
   `(magit-process-ng ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-log-author ((t (:foreground ,jp-darkvenom-orange-2))))
   `(magit-log-date ((t (:foreground ,jp-darkvenom-mono-2))))
   `(magit-log-graph ((t (:foreground ,jp-darkvenom-silver))))
   `(magit-sequence-pick ((t (:foreground ,jp-darkvenom-orange-2))))
   `(magit-sequence-stop ((t (:foreground ,jp-darkvenom-green))))
   `(magit-sequence-part ((t (:foreground ,jp-darkvenom-orange-1))))
   `(magit-sequence-head ((t (:foreground ,jp-darkvenom-blue))))
   `(magit-sequence-drop ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-sequence-done ((t (:foreground ,jp-darkvenom-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,jp-darkvenom-mono-2))))
   `(magit-bisect-good ((t (:foreground ,jp-darkvenom-green))))
   `(magit-bisect-skip ((t (:foreground ,jp-darkvenom-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-blame-heading ((t (:background ,jp-darkvenom-bg-1 :foreground ,jp-darkvenom-mono-2))))
   `(magit-blame-hash ((t (:background ,jp-darkvenom-bg-1 :foreground ,jp-darkvenom-purple))))
   `(magit-blame-name ((t (:background ,jp-darkvenom-bg-1 :foreground ,jp-darkvenom-orange-2))))
   `(magit-blame-date ((t (:background ,jp-darkvenom-bg-1 :foreground ,jp-darkvenom-mono-3))))
   `(magit-blame-summary ((t (:background ,jp-darkvenom-bg-1 :foreground ,jp-darkvenom-mono-2))))
   `(magit-dimmed ((t (:foreground ,jp-darkvenom-mono-2))))
   `(magit-hash ((t (:foreground ,jp-darkvenom-purple))))
   `(magit-tag  ((t (:foreground ,jp-darkvenom-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,jp-darkvenom-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,jp-darkvenom-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,jp-darkvenom-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,jp-darkvenom-blue :weight bold))))
   `(magit-refname        ((t (:background ,jp-darkvenom-bg :foreground ,jp-darkvenom-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,jp-darkvenom-bg :foreground ,jp-darkvenom-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,jp-darkvenom-bg :foreground ,jp-darkvenom-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,jp-darkvenom-green))))
   `(magit-signature-bad       ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,jp-darkvenom-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,jp-darkvenom-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,jp-darkvenom-purple))))
   `(magit-reflog-commit       ((t (:foreground ,jp-darkvenom-green))))
   `(magit-reflog-amend        ((t (:foreground ,jp-darkvenom-purple))))
   `(magit-reflog-merge        ((t (:foreground ,jp-darkvenom-green))))
   `(magit-reflog-checkout     ((t (:foreground ,jp-darkvenom-blue))))
   `(magit-reflog-reset        ((t (:foreground ,jp-darkvenom-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,jp-darkvenom-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,jp-darkvenom-green))))
   `(magit-reflog-remote       ((t (:foreground ,jp-darkvenom-cyan))))
   `(magit-reflog-other        ((t (:foreground ,jp-darkvenom-cyan))))

   ;; message
   `(message-cited-text ((t (:foreground ,jp-darkvenom-green))))
   `(message-header-cc ((t (:foreground ,jp-darkvenom-orange-1 :weight bold))))
   `(message-header-name ((t (:foreground ,jp-darkvenom-purple))))
   `(message-header-newsgroups ((t (:foreground ,jp-darkvenom-orange-2 :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,jp-darkvenom-red-1))))
   `(message-header-subject ((t (:foreground ,jp-darkvenom-blue))))
   `(message-header-to ((t (:foreground ,jp-darkvenom-orange-2 :weight bold))))
   `(message-header-xheader ((t (:foreground ,jp-darkvenom-silver))))
   `(message-mml ((t (:foreground ,jp-darkvenom-purple))))
   `(message-separator ((t (:foreground ,jp-darkvenom-mono-3 :slant italic))))

   ;; epa
   `(epa-field-body ((t (:foreground ,jp-darkvenom-blue :slant italic))))
   `(epa-field-name ((t (:foreground ,jp-darkvenom-cyan :weight bold))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,jp-darkvenom-blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,jp-darkvenom-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,jp-darkvenom-orange-1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,jp-darkvenom-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,jp-darkvenom-purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,jp-darkvenom-orange-2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,jp-darkvenom-blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,jp-darkvenom-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,jp-darkvenom-orange-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,jp-darkvenom-cyan))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,jp-darkvenom-purple))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,jp-darkvenom-orange-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))

   ;; show-paren
   `(show-paren-match ((,class (:foreground ,jp-darkvenom-purple :inherit bold :underline t))))
   `(show-paren-mismatch ((,class (:foreground ,jp-darkvenom-red-1 :inherit bold :underline t))))

   ;; sh-mode
   `(sh-heredoc ((t (:inherit font-lock-string-face :slant italic))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,jp-darkvenom-red-1 :background ,jp-darkvenom-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,jp-darkvenom-gray :weight bold))))

   ;; lispy
   `(lispy-face-hint ((t (:background ,jp-darkvenom-border :foreground ,jp-darkvenom-orange-2))))

   ;; lispyville
   `(lispyville-special-face ((t (:foreground ,jp-darkvenom-red-1))))

   ;; web-mode
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-red-1))))
   `(web-mode-html-attr-equal-face ((t (:inherit default))))
   `(web-mode-html-attr-name-face ((t (:foreground ,jp-darkvenom-orange-1))))
   `(web-mode-html-tag-bracket-face ((t (:inherit default))))
   `(web-mode-html-tag-face ((t (:foreground ,jp-darkvenom-red-1))))
   `(web-mode-symbol-face ((t (:foreground ,jp-darkvenom-orange-1))))

   ;; nxml
   `(nxml-attribute-local-name ((t (:foreground ,jp-darkvenom-orange-1))))
   `(nxml-element-local-name ((t (:foreground ,jp-darkvenom-red-1))))
   `(nxml-markup-declaration-delimiter ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
   `(nxml-processing-instruction-delimiter ((t (:inherit nxml-markup-declaration-delimiter))))

   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,jp-darkvenom-mono-2 :background ,jp-darkvenom-bg))))
   `(line-number-current-line ((t (:foreground ,jp-darkvenom-fg :background ,jp-darkvenom-bg))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,jp-darkvenom-gray))))
   `(reb-match-1 ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,jp-darkvenom-black :foreground ,jp-darkvenom-orange-2 :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face ((t (:underline (:color ,jp-darkvenom-red-1 :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face ((t (:inherit default))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,jp-darkvenom-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,jp-darkvenom-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,jp-darkvenom-green))))
   `(font-latex-warning-face ((t (:foreground ,jp-darkvenom-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,jp-darkvenom-cyan))))
   `(font-latex-script-char-face ((t (:foreground ,jp-darkvenom-mono-2))))

   ;; org-mode
   `(org-level-1 ((t (:foreground ,jp-darkvenom-mono-3))))
   `(org-level-2 ((t (:foreground ,jp-darkvenom-blue))))
   `(org-date ((t (:foreground ,jp-darkvenom-cyan))))
   `(org-ellipsis ((t (:underline nil))))
   `(org-document-info ((t (:foreground ,jp-darkvenom-mono-2))))
   `(org-document-info-keyword ((t (:inherit org-meta-line :underline nil))))
   `(org-document-title ((t (:height 1.5 :foreground ,jp-darkvenom-fg))))
   `(org-verbatim ((t (:foreground ,jp-darkvenom-mono-3))))
   ;; `(org-block-begin-line ((t (:background ,jp-darkvenom-border))))
   `(org-block ((t (:background ,jp-darkvenom-gutter))))
   ;; `(org-block-end-line ((t (:background ,jp-darkvenom-border))))
   `(org-footnote ((t (:foreground ,jp-darkvenom-cyan))))
   `(org-sexp-date ((t (:foreground ,jp-darkvenom-cyan))))
   `(org-todo ((t (:foreground ,jp-darkvenom-red-1))))
   `(org-checkbox-statistics-done ((t (:inherit org-todo))))
   `(org-checkbox ((t (:background ,jp-darkvenom-bg
								   :foreground ,jp-darkvenom-red-1
								   :box (:line-width 2 :color ,jp-darkvenom-mono-2
													 :style released-button)))))
   `(org-scheduled-previously ((t (:foreground ,jp-darkvenom-red-1))))
   `(org-scheduled ((t (:foreground ,jp-darkvenom-cyan))))
   `(org-scheduled-today ((t (:foreground ,jp-darkvenom-accent))))

   ;; calendar
   `(diary ((t (:inherit warning))))
   `(holiday ((t (:foreground ,jp-darkvenom-green))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,jp-darkvenom-orange-1))))
   `(breakpoint-enabled ((t (:foreground ,jp-darkvenom-red-1 :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,jp-darkvenom-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,jp-darkvenom-orange-1))   `(realgud-overlay-arrow2        ((t (:foreground ,jp-darkvenom-orange-2))))
									))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,jp-darkvenom-red-1)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,jp-darkvenom-gray)))))
   `(realgud-line-number           ((t (:foreground ,jp-darkvenom-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit ruler-mode-default))))
   `(ruler-mode-comment-column ((t (:foreground ,jp-darkvenom-red-1))))
   `(ruler-mode-current-column ((t (:foreground ,jp-darkvenom-accent :inherit ruler-mode-default))))
   `(ruler-mode-default ((t (:inherit mode-line))))
   `(ruler-mode-fill-column ((t (:foreground ,jp-darkvenom-orange-1 :inherit ruler-mode-default))))
   `(ruler-mode-fringes ((t (:foreground ,jp-darkvenom-green :inherit ruler-mode-default))))
   `(ruler-mode-goal-column ((t (:foreground ,jp-darkvenom-cyan :inherit ruler-mode-default))))
   `(ruler-mode-margins ((t (:inherit ruler-mode-default))))
   `(ruler-mode-tab-stop ((t (:foreground ,jp-darkvenom-mono-3 :inherit ruler-mode-default))))

   ))

(jp-darkvenom-with-color-variables
  (custom-theme-set-variables
   'jp-darkvenom
   ;; fill-column-indicator
   `(fci-rule-color ,jp-darkvenom-gray)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `jp-darkvenom-orange-2' |
   ;; | J         | `jp-darkvenom-blue'     |
   ;; | L         | `jp-darkvenom-orange-1' |
   ;; | Z         | `jp-darkvenom-red-1'    |
   ;; | S         | `jp-darkvenom-green'    |
   ;; | T         | `jp-darkvenom-purple'   |
   ;; | I         | `jp-darkvenom-cyan'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,jp-darkvenom-black ,jp-darkvenom-red-1 ,jp-darkvenom-green ,jp-darkvenom-orange-2
					   ,jp-darkvenom-blue ,jp-darkvenom-purple ,jp-darkvenom-cyan ,jp-darkvenom-fg])
   ))

(defvar jp-darkvenom-theme-force-faces-for-mode t
  "If t, jp-darkvenom-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Tomorrow Night
Theme from doom-emacs as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
* html-mode: font-lock-function-name-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Tomorrow Night Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Tomorrow Night Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `jp-darkvenom-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun jp-darkvenom-theme-change-faces-for-mode ()
  (interactive)
  (when (or jp-darkvenom-theme-force-faces-for-mode (called-interactively-p))
    (jp-darkvenom-with-color-variables
     (cond
      ((member major-mode '(js2-mode))
	   (face-remap-add-relative 'font-lock-constant-face :foreground jp-darkvenom-orange-1)
	   (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
	   (face-remap-add-relative 'font-lock-variable-name-face :foreground jp-darkvenom-mono-1))
      ((member major-mode '(html-mode))
	   (face-remap-add-relative 'font-lock-function-name-face :foreground jp-darkvenom-red-1)
	   (face-remap-add-relative 'font-lock-variable-name-face :foreground jp-darkvenom-orange-1))))))

(add-hook 'after-change-major-mode-hook 'jp-darkvenom-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'jp-darkvenom)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; jp-darkvenom-theme.el ends here
