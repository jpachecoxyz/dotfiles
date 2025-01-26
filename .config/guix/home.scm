(use-modules (gnu)
             (gnu home)
             (gnu home services dotfiles)
             (gnu home services shells))

(home-environment
 (packages (specifications->packages
            (list 
                  "git"
                  ;; fonts
                  "fontconfig"
                  "font-jetbrains-mono"
                  "font-ibm-plex"
                  "font-iosevka"
                  "font-nerd-fira-code"

                  ;;; GTK 3
                  "gtksourceview"
                  "materia-theme"

                  ;; Tools
                  "waybar"
                  "wl-clipboard"
                  "bat"
                  )))

 ;; (services (list 
 ;;                 (service home-dotfiles-service-type
 ;;                          (home-dotfiles-configuration
 ;;                           (directories '("../../files"))))))

 )
