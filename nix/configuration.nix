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
  boot.tmp.cleanOnBoot = true;
  networking.hostName = "nixos"; # Define your hostname.
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Trusted users 
  nix.settings.trusted-users = [ "root" "javier" ];

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Matamoros";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Configure keymap in X11
  services.xserver = {
    xkb.layout = "gb";
    xkb.variant = "";
  };

  # Configure console keymap
  console.keyMap = "uk";

  # sound.enable = true;
  # hardware.pulseaudio.enable = false;
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
  services.logind.lidSwitch = "ignore";

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
  # security.doas.enable = true;
  security.sudo = {
    enable = true;
    extraConfig = with pkgs; ''
      Defaults:picloud secure_path="${lib.makeBinPath [
        systemd
      ]}:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin"
    '';
  };

  # security.doas.extraRules = [{
  #   users = [ "javier" ];
  #   keepEnv = true;
  #   persist = true;
  # }];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # zsh
  programs.zsh = {
    enable = true;
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ 
      stdenv.cc.cc.lib
      zlib
    ];

  # users.defaultUserShell = pkgs.zsh;
  users.users.javier.shell = pkgs.zsh;

  # Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    iosevka
    jetbrains-mono
    font-awesome
    ibm-plex
    noto-fonts-color-emoji
    # gentium
    # google-fonts
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
    ];
  };

  # Hyprland
  programs.hyprland = {
    enable = true;
  };

  environment.systemPackages = (with pkgs; [
    # development
    # (pkgs.writeScriptBin "sudo" ''exec doas "$@"'')
    wget
    tdlib
    git
    nix-init
    nix-search-cli
    nix-index
    stow
    home-manager
    gnumake
    cmake

    # fonts
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    iosevka
    jetbrains-mono
    font-awesome
    ibm-plex
    noto-fonts-color-emoji

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
    # pinentryPackage = "curses";
    # pinentryPackage = pkgs.pinentry-qt;
    enableSSHSupport = true;
  };

  services.getty.autologinUser = "javier";

  services.locate.enable = true;

  services.upower = {
    enable = true;
  };
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

  # Enable ZRAM for swap
  zramSwap = {
    enable = true;
    memoryPercent = 100;
    algorithm = "zstd";
  };

}
