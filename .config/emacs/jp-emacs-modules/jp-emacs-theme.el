;;; Theme setup and related

;;;; Load the desired theme module
;; These all reference my packages: `modus-themes', `ef-themes',
;; `doric-themes', `standard-themes'.
(when jp-emacs-load-theme-family
  (require
   (pcase jp-emacs-load-theme-family
     ('modus 'jp-emacs-modus-themes)
     ('ef 'jp-emacs-ef-themes)
     ('doric 'jp-emacs-doric-themes)
     ('standard 'jp-emacs-standard-themes))))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(jp-emacs-configure
  (jp-emacs-install pulsar)

  (pulsar-global-mode 1)

  (jp-emacs-hook
    (next-error-hook minibuffer-setup-hook)
    (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))

  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta)

  (jp-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
    "C-x L" #'pulsar-highlight-permanently-dwim) ; or use `pulsar-highlight-temporarily-dwim'

  (customize-set-variable
   'pulsar-pulse-functions
    '(ace-window
      backward-page
      bookmark-jump
      delete-other-windows
      delete-window
      dired-maybe-insert-subdir
      dired-up-directory
      dired-goto-file
      dired-next-dirline
      dired-prev-dirline
      evil-goto-first-line
      evil-goto-line evil-scroll-down
      evil-scroll-line-to-bottom
      evil-scroll-line-to-center
      evil-scroll-line-to-top
      evil-scroll-page-down
      evil-scroll-page-up
      evil-scroll-up evil-window-next
      evil-delete
      evil-delete-char
      evil-delete-line
      evil-delete-marks
      evil-undo
      evil-window-up
      evil-window-new
      evil-window-top
      evil-window-down
      evil-window-next
      evil-window-left
      evil-window-right
      evil-window-bottom
      evil-window-vsplit
      evil-window-split
      evil-window-new
      evil-window-next
      evil-window-delete
      forward-page
      goto-line handle-switch-frame imenu
      logos-backward-page-dwim
      logos-forward-page-dwim
      handle-select-window
      move-to-window-line-top-bottom
      narrow-to-defun
      narrow-to-page narrow-to-region
      next-buffer
      next-error next-error-recenter
      next-multiframe-window
      occur-mode-goto-occurrence
      org-backward-heading-same-level
      org-forward-heading-same-level org-next-visible-heading
      org-previous-visible-heading other-window
      outline-backward-same-level outline-forward-same-level
      outline-next-visible-heading
      outline-previous-visible-heading outline-up-heading
      previous-buffer previous-error quit-window
      recenter-top-bottom reposition-window scroll-down-command
      scroll-up-command tab-close tab-new tab-next tab-previous
      widen
      windmove-down
      windmove-left
      windmove-right
      windmove-swap-states-down
      windmove-swap-states-left
      windmove-swap-states-right
      windmove-swap-states-up
      windmove-up
      occur-mode-mouse-goto goto-next-locus
      next-match
      Info-exit next-window-any-frame
      tab-bar-close-tab
      tab-bar-new-tab
      tab-bar-switch-to-next-tab
       tab-bar-switch-to-prev-tab))
  
  (customize-set-variable
   'pulsar-pulse-region-functions
       '(backward-kill-paragraph
           backward-kill-sentence
           backward-kill-sexp
           backward-kill-word
           delete-region
           evil-yank
           evil-yank-line
           kill-line
           kill-paragraph
           kill-rectangle
           kill-region
           kill-ring-save
           kill-sentence
           kill-sexp
           kill-visual-line
           kill-whole-line
           kill-word
           open-rectangle
           undo yankyank-rectangle)))

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(jp-emacs-configure
  (jp-emacs-install lin)
  (setq lin-face 'lin-cyan)
  (lin-global-mode 1)
  (when (and (null jp-emacs-load-theme-family)
             (string= (getenv "DESKTOP_SESSION") "gnome"))
    (lin-gnome-accent-color-mode 1)))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
;; (when jp-display-graphic-p
  (jp-emacs-configure
    (jp-emacs-install spacious-padding)

    (spacious-padding-mode 1)

    (setq spacious-padding-widths
          `( :internal-border-width 15
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 15
             :scroll-bar-width ,(if jp-pgtk-p 12 6)
             :left-fringe-width 20
             :right-fringe-width 20))

    ;; (setq spacious-padding-subtle-mode-line nil)

    ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
    ;; is very flexible.  Here we make the mode lines be a single
    ;; overline, while header lines have an underline.
    (setq spacious-padding-subtle-frame-lines
          '( :mode-line-active spacious-padding-line-active
             :mode-line-inactive spacious-padding-line-inactive
             :header-line-active spacious-padding-line-active
             :header-line-inactive spacious-padding-line-inactive))

    (when (< emacs-major-version 29)
      (setq x-underline-at-descent-line (when spacious-padding-subtle-frame-lines t))))
;; )

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(jp-emacs-configure
  (jp-emacs-install fontaine)

  (fontaine-mode 1)

  (jp-emacs-keybind global-map
    "C-c f" #'fontaine-set-preset
    "C-c F" #'fontaine-toggle-preset)

  (setq-default text-scale-remap-header-line t) ; Emacs 28

  ;; The font family is my design: <https://github.com/protesilaos/aporetic>.
  (setq fontaine-presets
        '((small
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "IBM Plex Mono"
        :variable-pitch-family "Aporetic Serif Mono"
        :default-height 80)

        (regular
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "IBM Plex Mono"
        :variable-pitch-family "Aporetic Serif Mono"
        :default-height 120)

        (medium
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "IBM Plex Mono"
        :variable-pitch-family "Aporetic Serif Mono"
        :default-height 145)

        (large
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "IBM Plex Mono"
        :variable-pitch-family "Aporetic Serif Mono"
        :default-height 160)

        (presentation
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "IBM Plex Mono"
        :variable-pitch-family "Aporetic Serif Mono"
        :default-height 180)

        (jumbo
        :inherit medium
        :default-height 260)

        (t
        :default-family "Aporetic Serif Mono"
        :fixed-pitch-family "CaskaydiaMono Nerd Font"
        :variable-pitch-family "Aporetic Serif Mono")))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(when jp-display-graphic-p
  (jp-emacs-configure
    (jp-emacs-install show-font)

    (setq show-font-display-buffer-action-alist '(display-buffer-full-frame))

    (jp-emacs-keybind global-map
      "C-c S s" #'show-font-select-preview
      "C-c S l" #'show-font-tabulated)))

;;;;; `variable-pitch-mode' setup
(jp-emacs-configure
  (define-key ctl-x-x-map (kbd "v") #'variable-pitch-mode)

  (defun jp/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (when (bound-and-true-p modus-themes-mixed-fonts)
        (variable-pitch-mode 1))))

  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (jp-emacs-hook
    (text-mode-hook notmuch-show-mode-hook elfeed-show-mode-hook)
    jp/enable-variable-pitch)
;;;;; Resize keys with global effect

  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (jp-emacs-keybind global-map
    "C-x C-=" #'global-text-scale-adjust
    "C-x C-+" #'global-text-scale-adjust
    "C-x C-0" #'global-text-scale-adjust))
(global-visual-line-mode 1)

(provide 'jp-emacs-theme)
