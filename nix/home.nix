#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓         ╻ ╻┏━┓┏┳┓┏━╸ ┏┓╻╻╻ ╻
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ╺━╸   ┣━┫┃ ┃┃┃┃┣╸  ┃┗┫┃┏╋┛
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸         ╹ ╹┗━┛╹ ╹┗━╸╹╹ ╹╹╹ ╹

{ config, pkgs, pkgs-unstable, overlays, ... }:
  let
    # Override ncmpcpp with the desired features
    myNcmpcpp = pkgs.ncmpcpp.override {
      visualizerSupport = true;
      clockSupport = true;
    };
  in

{
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
    pkgs.wbg

    # Development
    # language servers
    pkgs.pyright
    pkgs.poetry
    pkgs.rye
    pkgs.lua-language-server
    pkgs.arduino-ide
    pkgs.hugo
    pkgs.gcc
    pkgs.neovim
    # pkgs-unstable.emacs-gtk
    pkgs.emacs-git
    pkgs-unstable.tdlib
    pkgs.python3
    pkgs.nodejs
    pkgs.pagefind
    pkgs.cargo
    pkgs.nil
    pkgs.direnv

    # Terminal tools
    pkgs.yazi   # File manager
    pkgs.zellij # Terminal multiplexer
    pkgs.lazygit # git tui frontend
    pkgs.fossil

    # Generic tools.
    pkgs.graphite-gtk-theme 
    pkgs.breeze-icons
    pkgs.bibata-cursors
    pkgs.brightnessctl
    pkgs.nwg-look
    pkgs.libnotify
    pkgs.yad
    pkgs.waybar
    pkgs.wl-clipboard
    pkgs.cliphist
    pkgs.grimblast
    pkgs.mako
    pkgs.foot
    pkgs.swaylock-effects
    pkgs.cron
    pkgs.stow
    pkgs.tree
    pkgs.fzf
    pkgs.eza
    pkgs.fd
    pkgs.jq
    pkgs.ripgrep
    pkgs.bat
    pkgs.ffmpeg
    pkgs.fastfetch
    pkgs.wf-recorder
    pkgs.tofi
    pkgs.transmission-gtk

    # Browser / web
    pkgs.nyxt
    pkgs.firefox

    # multimedia
    myNcmpcpp
    pkgs.pulseaudioFull
    pkgs.mpd
    pkgs.mpc-cli
    pkgs.mpv
    pkgs.ytfzf
    pkgs.pulsemixer
    pkgs.spotdl
    pkgs.yt-dlp
    pkgs.obs-studio
    pkgs.telegram-desktop

    # graphics
    pkgs.nsxiv
    pkgs.slurp
    pkgs.xorg.xrdb

    # tools
    pkgs.poppler
    pkgs.zathura
    pkgs.unzip
    pkgs.zip
    pkgs.killall
    pkgs.htop

    # latex and spell
    pkgs.hunspell
    pkgs.hunspellDicts.en_US
    pkgs.hunspellDicts.es_MX
    pkgs.tectonic

    # privacy
    pkgs.tomb
    pkgs.qrencode
    pkgs.steghide
    pkgs.asciicam
    pkgs.pinentry-curses
    pkgs.pass
    pkgs.gnupg

    # Custom packages
    (pkgs.callPackage ../jp-nix/whdd/default.nix { })

  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    
  };

  # home.sessionVariables = {
  #   XCURSOR_THEME = "Bibata-Modern-Classic";
  #   XCURSOR_SIZE = "22"; # Adjust the size as needed
  # };

  # home.pointVerCursor = {
  #   gtk.enable = true;
  #   package = pkgs.bibata-cursors;
  #   name = "Bibata-Modern-Classic";
  #   size = 22;
  #   };

  # zsh
  # programs.zsh = {
  #   enable = true;
  #   plugins = [
  #     {
  #       name = "zsh-syntax-highlighting";
  #       src = pkgs.zsh-syntax-highlighting;
  #     }
  #     {
  #       name = "zsh-autosuggestions";
  #       src = pkgs.zsh-autosuggestions;
  #     }
  #   ];
  #   dotDir = ".config/test";
  # };

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
    cursorTheme.size = 22;
    theme.package = pkgs.gruvbox-dark-gtk;
    theme.name = "gruvbox-dark";
    # gtk3.extraConfig = {
    #   Settings = ''
    #     gtk-application-prefer-dark-theme=1
    #   '';
    # };
    iconTheme = {
      package = pkgs.gruvbox-dark-icons-gtk;
      name  = "gruvbox-dark-icons";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

# emcas lastest version
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      sha256 = "1z3v3h8cmd9465l9sa2d4hzihdvbx204p2fckanl5s76gg9gddgy";
    }))
  ];
  services.emacs.enable = true;
}
