#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓         ╻ ╻┏━┓┏┳┓┏━╸ ┏┓╻╻╻ ╻
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ╺━╸   ┣━┫┃ ┃┃┃┃┣╸  ┃┗┫┃┏╋┛
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸         ╹ ╹┗━┛╹ ╹┗━╸╹╹ ╹╹╹ ╹

{ inputs, config, pkgs, pkgs-unstable, lib, ... }:

let
  # Override ncmpcpp with the desired features
  myNcmpcpp = pkgs.ncmpcpp.override {
    visualizerSupport = true;
    clockSupport = true;
  };
  whdd = pkgs.callPackage ../jp-nix/whdd/default.nix { };
  libastal = inputs.astal.packages.${pkgs.system};

in

{
  
  imports = [ 
  # inputs.ags.homeManagerModules.default
              inputs.textfox.homeManagerModules.default 
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

  textfox = {
      enable = true;
      profile = "javier";
      config = {
        background = {
          color = "#1e1e1e";
        };
        border = {
          color = "#458588";
          width = "2px";
          transition = "1.0s ease";
          radius = "3px";
        };
        displayHorizontalTabs = true;
        font = { 
          family = "Iosevka";
          size = "15px";
          accent = "#458588";
        };
        sidebery = {
          margin = "1.0rem";
        };
      };
  };

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
    pkgs.swww
    pkgs.ags
    pkgs-unstable.gtksourceview
    pkgs-unstable.accountsservice
    pkgs.libdbusmenu-gtk3

    # AGS
    pkgs.dart-sass
    pkgs.gnome.gvfs
    pkgs.libgtop
    pkgs.gobject-introspection
    pkgs.cava
    pkgs.bun
    pkgs.fd
    pkgs.hyprpicker
    pkgs.wayshot
    pkgs.gjs
    # libastal.astal
    # libastal.battery
    # libastal.auth
    # libastal.apps
    # libastal.tray

    # Development
    # language servers
    pkgs.pyright
    pkgs.ruff
    pkgs.rye
    pkgs.lua-language-server
    pkgs.hugo
    pkgs.javascript-typescript-langserver
    pkgs.gcc
    pkgs.neovim
    pkgs-unstable.emacs30
    # pkgs.emacs-git
    pkgs-unstable.tdlib
    pkgs.python3
    pkgs.nodejs
    pkgs.pagefind
    pkgs.cargo
    pkgs.nil
    pkgs.direnv

    # AI Models
    pkgs.ollama

    # Terminal tools
    pkgs-unstable.yazi    # File manager
    pkgs.zellij           # Terminal multiplexer
    pkgs.lazygit          # git tui frontend
    pkgs.gtk3-x11         # gtk-launcher

    # Generic tools.
    pkgs.graphite-gtk-theme 
    pkgs.breeze-icons
    pkgs.bibata-cursors
    pkgs.brightnessctl
    pkgs.libnotify
    pkgs.waybar
    pkgs.wl-clipboard
    pkgs.cliphist
    pkgs.grimblast
    pkgs.mako
    pkgs.foot
    pkgs.kitty
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
    pkgs.tofi
    pkgs.transmission-gtk
    pkgs.wlsunset

    # Browser / web
    pkgs.qutebrowser
    pkgs.firefox
    pkgs.google-chrome
    # pkgs.nyxt

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
    pkgs.kdenlive
    pkgs-unstable.gowall

    # graphics
    pkgs.nsxiv
    pkgs.slurp
    pkgs.swappy
    pkgs.hyprshot
    pkgs.grim
    pkgs.xorg.xrdb

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
    pkgs.btop

    # latex and spell
    pkgs.emacsPackages.jinx
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
    whdd

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
      # package = pkgs.gruvbox-dark-icons-gtk;
      # name  = "gruvbox-dark-icons";
      package = pkgs.papirus-icon-theme;  # Use Papirus icon theme package
      name  = "Papirus";                  # Set the theme name to Papirus
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # emcas lastest version
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  #     sha256 = "sha256:0sd2jlismvl07i9c9ks97d77370s3lnjglxsg96g3p7gz2l1nm7x";
  #   }))
  # ];

  # Example of how to pin a version of a packagein the overlays, you must have the commit in the url:
  # url = "https://github.com/nix-community/emacs-overlay/archive/<commit-or-tag>.tar.gz";
  # Example:
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = "https://github.com/nix-community/emacs-overlay/archive/d2f8eae4.tar.gz";  # Pinned commit
  #     sha256 = "sha256:1wm92zn2z8y7kdcn0b91z7h63ydw3vxy6vbd9wzl1kpxx5m68dd8";
  #   }))
  # ];

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
