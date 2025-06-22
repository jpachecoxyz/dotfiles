#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓   ┏┓╻╻╻ ╻   ┏━┓┏━┓┏━╸╻┏ ┏━┓┏━╸┏━╸┏━┓
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ┃┗┫┃┏╋┛╺━╸┣━┛┣━┫┃  ┣┻┓┣━┫┃╺┓┣╸ ┗━┓
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸   ╹ ╹╹╹ ╹   ╹  ╹ ╹┗━╸╹ ╹╹ ╹┗━┛┗━╸┗━┛

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

{
  environment.systemPackages = [

    # Environment
    swww
    hugo
    yazi
    zellij
    ags
    gtksourceview
    accountsservice
    libdbusmenu-gtk3
    dart-sass
    gobject-introspection
    bun
    fd
    gjs
    nwg-look

    # Fonts
    nerd-fonts.jetbrains-mono
    noto-fonts-color-emoji
    bibata-cursors
    # fontconfig

    # Browsers
    google-chrome
    firefox-wayland

    # Text editors
    neovim
    emacs30
    emacsPackages.jinx
    emacsPackages.vterm
    enchant
    hunspell
    hunspellDicts.es_MX
    hunspellDicts.en_US

    # Tools
    mako
    grimblast
    slurp
    swappy
    grim
    libnotify
    fastfetch
    direnv
    eza
    nnn
    ripgrep
    jq
    wf-recorder
    htop
    xorg.xrdb
    nsxiv
    unzip
    p7zip
    lazygit
    spotdl
    yt-dlp
    nix-search
    qbittorrent
    ncurses
    pinentry-all

    # multimedia
    ffmpeg
    mpv
    pulsemixer
    telegram-desktop

    # TODO PDF-latex tools
    tectonic

    # Privacy
    tomb
    qrencode
    steghide
    gnupg
    ];
}
