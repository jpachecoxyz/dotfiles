;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu) 
             (nongnu packages linux)
             (gnu packages shells)
             (gnu services linux)
             (gnu system))

(use-service-modules desktop 
                     nix
                     networking 
                     admin
                     ssh)

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
                (supplementary-groups '("wheel" "netdev" "audio" "video" )))
               %base-user-accounts))

 ;; System packages
 (packages (append (map specification->package
			'(;; Utils
                          "git"
                          "vim"
                          "stow"
                          "gcc"
                          "brightnessctl"
                          "ncurses"
                          "pinentry-tty"
                          "curl"
                          "file"
                          "gnupg"
                          "dbus"
                          "bluez"

                          ;; WM
                          "sway"
                          "hyprland"
                          "dwl"
                          "foot"
                          "tofi"

                          ;; Audio
                          "wireplumber"
                          "pipewire"
                          "pulseaudio"))
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
           ;; (service seatd-service-type)
           (service elogind-service-type)
           (service ntp-service-type)

          polkit-wheel-service

           ;; Enable the build service for Nix package manager
           (service nix-service-type)

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
              (keyboard-layout keyboard-layout)
              (menu-entries
               (list
                (menu-entry
                 (label "Windows 11")
                 (device (uuid "65A0-06CA" 'fat32))
                 (chain-loader "/EFI/Microsoft/Boot/bootmgfw.efi"))))
			  ))

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
