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
    "C-x L" #'pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(jp-emacs-configure
  (jp-emacs-install lin)
  (setq lin-face 'lin-cyan)
  (lin-global-mode 1))

;;;; Increase padding of windows/frames
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
    "C-x C-0" #'global-text-scale-adjust)
(global-visual-line-mode 1)

(provide 'jp-emacs-theme)
