;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu) (nongnu packages linux) (gnu packages shells))
(use-service-modules desktop networking ssh )

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Matamoros")
  (keyboard-layout (keyboard-layout "gb"))
  (host-name "guix")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "javier")
                  (comment "Javier Pacheco")
                  (group "users")
                  (home-directory "/home/javier")
		  (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; System packages
  (packages (append (map specification->package
			 '(;; Utils
                          "git"
                          "stow"
                          "gcc"
                          "brightnessctl"
                          "ncurses"
                          "pinentry"
                          "gnupg"
                          "dbus"
                          "bluez"

                          ;; fonts
                          "fontconfig"
                          "font-jetbrains-mono"
                          "font-ibm-plex"
                          "font-iosevka"
                          "font-nerd-fira-code"

                          ;; Text editors
                          "neovim"
                          "emacs"
                          "emacs-jinx"
                          "enchant"
                          "hunspell"
                          "hunspell-dict-en-us"

                          ;; Environment
                          "hyprland"
                          "kitty"
                          "waybar"
                          "swww"
                          "tofi"
                          "mako"
                          "grimblast"
                          "slurp"
                          "swappy"
                          ;; "hyprshot" ;; Not found in guix
                          "grim"
                          "libnotify"
                          "materia-theme"

                          ;; Audio
                          "wireplumber"
                          "pipewire"
                          "pulseaudio"

                          ;; Tools
                          "fastfetch" ;; Useless things
                          "fzf"
                          "direnv"
                          "eza"
                          "wl-clipboard"
                          ;; "yazi" ;; Not found in guix
                          "nnn"
                          "bat"
                          "ripgrep"
                          "jq"
                          "wf-recorder"
                          "htop"
                          "xrdb"
                          "nsxiv"

                          ;; multimedia
                          "mpd"
                          "ncmpcpp"
                          ;; "mpd-cli" :; Not found in guix
                          "mpv"
                          "pulsemixer"
                          ;; "spotdl" ;; Not found in guix
                          "yt-dlp"
                          "telegram-desktop"

                          ;; TODO PDF-latex tools

                          ;; Privacy
                          "tomb"
                          "qrencode"
                          "steghide"
                          "gnupg"

                          ;; Languages:
                          "python"
                          ;; "python-pip"
                          "node"

                          ;; Browsers
                          "zen-browser-bin-generic"
			   ))
		    %base-packages))


  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list

                ;; To configure OpenSSH, pass an 'openssh-configuration'
                ;; record as a second argument to 'service' below.
                (service openssh-service-type)
                (service network-manager-service-type)
                (service wpa-supplicant-service-type)
                (service elogind-service-type)
                (service ntp-service-type)

                ;; (service zram-device-service-type
                ;;      (zram-device-configuration
                ;;       (size (* 16 (expt 2 30)))
                ;;       (compression-algorithm 'zstd)
                ;;       (priority 100)))

                (service bluetooth-service-type
                       (bluetooth-configuration
                        (name host-name)
                        (auto-enable? #t)
                        (name-resolving? #t)
                        (fast-connectable? #t)
                        (always-pairable? #t)
                        (multi-profile 'multiple))))

           ;; This is the default list of services we
           ;; are appending to.
           %base-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "9eb1ab9f-a7d5-42b9-b3b9-d98daa6c8dad"
                                  'xfs))
                         (type "xfs"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "65A0-06CA"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
