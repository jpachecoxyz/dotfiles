#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓         ╻ ╻┏━┓┏┳┓┏━╸ ┏┓╻╻╻ ╻
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ╺━╸   ┣━┫┃ ┃┃┃┃┣╸  ┃┗┫┃┏╋┛
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸         ╹ ╹┗━┛╹ ╹┗━╸╹╹ ╹╹╹ ╹

{ config, pkgs, pkgs-unstable, hyprland, hyprland-plugins, ... }:

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

    # Browser / web
    pkgs.nyxt

    # Development
    # language servers
    pkgs.pyright
    pkgs.lua-language-server
    pkgs.hugo

    # Terminal tools
    pkgs.yazi   # File manager
    pkgs.zellij # Terminal multiplexer

    # Generic tools.
    # pkgs-unstable.gparted
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    
  };

  home.sessionVariables = {
    XCURSOR_THEME = "Bibata-Modern-Classic";
    XCURSOR_SIZE = "22"; # Adjust the size as needed
  };

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
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    iconTheme = {
      package = pkgs.gruvbox-dark-icons-gtk;
      name  = "gruvbox-dark-icons";
    };
  };

 #  gtk = {
 #    enable = true;
 #    font = {
 #      package = (pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; });
 #      name = "Mononoki Nerd Font Regular";
 #      size = 10;
 #    };
	#
 #    theme = {
 #      name = "Catppuccin-Macchiato-Compact-Pink-Dark";
 #      package = pkgs.catppuccin-gtk.override {
 #        accents = [ "pink" ];
 #        size = "compact";
 #        tweaks = [ "rimless" "black" ];
 #        variant = "macchiato";
 #      };
 #    };
	#
 #    iconTheme = {
 #      package = (pkgs.catppuccin-papirus-folders.override { flavor = "mocha"; accent = "pink"; });
 #      name  = "Papirus-Dark";
 #    };
	#
 #    cursorTheme = {
 #      name = "Catppuccin-Mocha-Pink";
 #      package = pkgs.catppuccin-cursors.mochaPink;
 #    };
	#
 #    gtk3.extraConfig = {
 #      Settings = ''
 #        gtk-application-prefer-dark-theme=1
 #      '';
 #    };
	#
 #    gtk4.extraConfig = {
 #      Settings = ''
 #        gtk-application-prefer-dark-theme=1
	# gtk-cursor-theme-name=Catppuccing-Mocha-Pink
 #      '';
 #    };
 #  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
