#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓         ╻ ╻┏━┓┏┳┓┏━╸ ┏┓╻╻╻ ╻
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ╺━╸   ┣━┫┃ ┃┃┃┃┣╸  ┃┗┫┃┏╋┛
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸         ╹ ╹┗━┛╹ ╹┗━╸╹╹ ╹╹╹ ╹

{ inputs, config, pkgs, pkgs-unstable, lib, overlays, ... }:

let
  # Override ncmpcpp with the desired features
  myNcmpcpp = pkgs.ncmpcpp.override {
    visualizerSupport = true;
    clockSupport = true;
  };
  # whdd = pkgs.callPackage ../jp-nix/whdd/default.nix { };
  # opencv-qt = pkgs.callPackage ../jp-nix/opencv-qt/default.nix { };
  libastal = inputs.astal.packages.${pkgs.system};

in

{

  imports = [
  # inputs.ags.homeManagerModules.default
  # inputs.textfox.homeManagerModules.default
  ];

  # programs.ags = {
  #   enable = true;
  #
  #   # additional packages to add to gjs's runtime
  #   extraPackages = with pkgs-unstable; [
  #     gtksourceview
  #     accountsservice
  #   ];
  # };

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "javier";
  home.homeDirectory = "/home/javier";

  # release notes.

  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # Environment
    pkgs.hypridle
    pkgs.hyprlock
    pkgs.pyprland
    pkgs.qtile-unwrapped
    # pkgs.swww

    # AGS
    # pkgs.ags
    # pkgs-unstable.gtksourceview
    # pkgs-unstable.accountsservice
    # pkgs.libdbusmenu-gtk3
    # pkgs.dart-sass
    # pkgs.gnome.gvfs
    # pkgs.libgtop
    # pkgs.gobject-introspection
    # pkgs.cava
    # pkgs.bun
    # pkgs.fd
    # pkgs.hyprpicker
    # pkgs.wayshot
    # pkgs.gjs

    # Development
    # language servers
    pkgs.pyright
    pkgs.ruff
    # pkgs.uv
    pkgs.lua-language-server
    pkgs.hugo
    pkgs.typst
    pkgs.tinymist
    pkgs.gcc
    pkgs.neovim
    # pkgs-unstable.emacs30
    pkgs.emacs-git
    pkgs-unstable.tdlib
    # pkgs.python3
    pkgs.nodejs
    pkgs.pagefind
    pkgs.cargo
    pkgs.nil
    pkgs.direnv
    # pkgs.devenv

    # AI Models
    pkgs.ollama

    # Terminal tools
    pkgs.lazygit          # git tui frontend
    # pkgs-unstable.yazi    # File manager
    # pkgs.zellij           # Terminal multiplexer
    # pkgs.gtk3-x11         # gtk-launcher

    # Generic tools.
    pkgs.graphite-gtk-theme
    # pkgs.breeze-icons
    pkgs.bibata-cursors
    pkgs.brightnessctl
    pkgs.libnotify
    pkgs.waybar
    pkgs.wl-clipboard
    pkgs.cliphist
    pkgs.grimblast
    pkgs.mako
    pkgs.foot
    pkgs.cron
    pkgs.tree
    pkgs.fzf
    pkgs.sysz
    pkgs.eza
    pkgs.fd
    pkgs.jq
    pkgs.ripgrep
    pkgs.bat
    pkgs.ffmpeg
    pkgs.imagemagick
    pkgs.fastfetch
    pkgs.wf-recorder
    pkgs.fuzzel

    # Browser / web
    pkgs.qutebrowser
    pkgs.firefox
    pkgs.google-chrome
    # pkgs.nyxt

    # multimedia
    # myNcmpcpp
    pkgs.pulseaudioFull
    # pkgs.mpd
    # pkgs.mpc-cli
    pkgs.mpv
    pkgs.ytfzf
    pkgs.pulsemixer
    pkgs.spotdl
    pkgs.yt-dlp
    # pkgs.obs-studio

    # graphics
    # pkgs.slurp
    pkgs.swappy
    pkgs.hyprshot
    pkgs.grim

    # Design
    pkgs.freecad-wayland

    # tools
    pkgs.poppler
    pkgs.mupdf
    pkgs.zathura
    pkgs.zathuraPkgs.zathura_pdf_mupdf
    pkgs.zathuraPkgs.zathura_pdf_poppler
    pkgs.unzip
    pkgs.zip
    pkgs.p7zip
    pkgs.killall
    pkgs.htop
    # pkgs.btop
    pkgs.fuse
    pkgs.pipx
    pkgs.cairo
    pkgs.pkgconf
    pkgs.gobject-introspection

    # latex and spell
    # These packages are most for emacs to write documents.
    pkgs.emacsPackages.jinx
    pkgs.emacsPackages.vterm
    pkgs.texlivePackages.xelatex-dev
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.es_MX
    pkgs.tectonic
    pkgs.python312Packages.pygments

    # privacy
    pkgs.tomb
    pkgs.qrencode
    pkgs.steghide
    pkgs.asciicam
    pkgs.pinentry-curses
    pkgs.pass
    pkgs.gnupg

    # Custom packages
    # whdd
    # opencv-qt
    # hints

  ];
  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # DOING Startto migrate my dotfiles, currently using GNU/stow
  home.file = {
  };

  # git config.
  programs.git = {
    enable = true;
    userName = "Javier Pacheco";
    userEmail = "javier@jpacheco.xyz";
    extraConfig = {
      init.DefaultBranch = "main";
    };
  };

  # Themes GTK
  gtk = {
    enable = true;

    cursorTheme.package = pkgs.bibata-cursors;
    cursorTheme.name = "Bibata-Modern-Classic";
    cursorTheme.size = 20;
    theme.package = pkgs.gruvbox-dark-gtk;
    theme.name = "gruvbox-dark";
    iconTheme = {
      package = pkgs.papirus-icon-theme;  # Use Papirus icon theme package
      name  = "Papirus";                  # Set the theme name to Papirus
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Emacs, lastest version
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      sha256 = "sha256:090h9qm8dqq3m7hq7ahc0fw8klzpc9c0xbkikl2ydb6z252my13c";
    }))
  ];

  # Enable the Emacs daemon service
  services.emacs = {
    enable = true;
  };

  # Nixpkgs configuration
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  # Custom scripts:
  # These content is not nesecary yet, I still manage my scripts using stow and
  # my personal dotfiles. I'm not fully migrate (and dont thing so) my scripts
  # to nix.

  # This script auto-tangle a org file for some shell-scripts that requires a .desktop file.
  # Step 1: Create the actual script
  # home.file.".local/bin/tangle-org.sh" = {
  #   text = ''
  #     #!/bin/sh
  #     ${pkgs-unstable.emacs30}/bin/emacs --batch \
  #           --eval "(require 'org)" \
  #           --eval "(progn (find-file \"~/.dotfiles/.emacs.d/lisp/applications.org\") (org-babel-tangle))"
  #   '';
  #   executable = true;
  # };
  #
  # # Step 2: Run the script during home-manager activation
  # home.activation.tangleOrgFile = ''
  #   # Run the tangle script
  #   ~/.local/bin/tangle-org.sh
  # '';
  #
}
