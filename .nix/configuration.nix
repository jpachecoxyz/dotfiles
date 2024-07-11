#  ┏┓┏━┓╻ ╻╻┏━╸┏━┓   ┏━╸┏━┓┏┓╻┏━╸╻┏━╸╻ ╻┏━┓┏━┓╺┳╸╻┏━┓┏┓╻ ┏┓╻╻╻ ╻
#   ┃┣━┫┃┏┛┃┣╸ ┣┳┛   ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓┃ ┃┣┳┛┣━┫ ┃ ┃┃ ┃┃┗┫ ┃┗┫┃┏╋┛
# ┗━┛╹ ╹┗┛ ╹┗━╸╹┗╸   ┗━╸┗━┛╹ ╹╹  ╹┗━┛┗━┛╹┗╸╹ ╹ ╹ ╹┗━┛╹ ╹╹╹ ╹╹╹ ╹

{ config, pkgs, pkgs-unstable, ... }:

let
  unstable = import <unstable> { };
in 

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Matamoros";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Configure keymap in X11
  services.xserver = {
    layout = "gb";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "uk";

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  # Power button disable:
  services.logind.powerKey = "ignore";
  services.logind.lidSwitch = "suspend";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.javier = {
    isNormalUser = true;
    description = "Javier";
    extraGroups = [ "networkmanager" "wheel" "audio"];
    packages = with pkgs; [];
  };

  # security.sudo.wheelNeedsPassword = false;
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Doas instead of sudo
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [{
    users = [ "javier" ];
    keepEnv = true;
    persist = true;
  }];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # zsh
  programs.zsh = {
    enable = true;
  };

  # users.defaultUserShell = pkgs.zsh;
  users.users.javier.shell = pkgs.zsh;

  # Fonts
  fonts.packages = with pkgs; [

    jetbrains-mono
    iosevka
    nerdfonts
    font-awesome
    ibm-plex
    noto-fonts-color-emoji
  ];

  # Hyprland
  programs.hyprland.enable = true; 

  environment.systemPackages = (with pkgs; [
    # development
    (pkgs.writeScriptBin "sudo" ''exec doas "$@"'')
    gcc
    wget
    git
    neovim
    emacs-gtk
    python3
    nodejs
    cargo
    nil
    direnv

    # enviroment
    ags
    gtk3
    libdbusmenu-gtk3
    graphite-gtk-theme 
    breeze-icons
    bibata-cursors
    nwg-look
    libnotify
    networkmanager
    waybar
    wl-clipboard
    pyprland
    grimblast
    mako
    foot
    swaylock-effects
    swww
    cron
    stow
    tree
    fzf
    eza
    fd
    jq
    ripgrep
    bat
    ffmpeg
    fastfetch
    wf-recorder
    tofi

    # web
    firefox
    google-chrome

    # multimedia
    ncmpcpp
    pulseaudioFull
    mpd
    mpc-cli
    mpv
    ytfzf
    pulsemixer
    spotdl
    yt-dlp

    # fonts
    jetbrains-mono
    nerdfonts
    font-awesome
    iosevka
    ibm-plex
    noto-fonts-color-emoji

    # graphics
    nsxiv
    slurp
    xorg.xrdb

    # tools
    nnn
    poppler
    zathura
    unzip
    zip
    killall
    htop

    # latex and spell
    hunspell
    hunspellDicts.en_US
    hunspellDicts.es_MX
    tectonic

    # privacy
    pinentry-curses
    pass
    gnupg

  ])
    
  ++
  
  (with pkgs-unstable; [
    # List of unstable packages going here...
    # vim
    
  ]);

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    #pinentryPackage = "curses";
    enableSSHSupport = true;
  };

  services.getty.autologinUser = "javier";
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
