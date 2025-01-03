(use-modules (gnu)
             (gnu home)
             (gnu home services dotfiles)
             (gnu home services shells))

(home-environment
 (packages (specifications->packages
            (list "git"
                  ;; fonts
                  "fontconfig"
                  "font-jetbrains-mono"
                  "font-ibm-plex"
                  "font-iosevka"
                  "font-nerd-fira-code"

                  ;; Text editors
                  "neovim"
                  "emacs"

                  ;; Spelling
                  "emacs-jinx"
                  "enchant"
                  "hunspell"
                  "hunspell-dict-en-us"

                  ;; Environment
                  "hyprlock"
                  "waybar"
                  "swww"
                  "mako"
                  "grimblast"
                  "slurp"
                  "swappy"
                  "grim"
                  "libnotify"
                  "materia-theme"

                  ;; Tools
                  "fastfetch" ;; Useless things
                  "fzf"
                  "direnv"
                  "eza"
                  "wl-clipboard"
                  "nnn"
                  "bat"
                  "ripgrep"
                  "jq"
                  "wf-recorder"
                  "htop"
                  "xrdb"
                  "nsxiv"
                  "unzip"
                  "p7zip"

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
                  "node"

                  ;; Browsers
                  "qutebrowser"
                  "librewolf"



                  )))

 ;; (services (list 
 ;;                 (service home-dotfiles-service-type
 ;;                          (home-dotfiles-configuration
 ;;                           (directories '("../../files"))))))

 )
