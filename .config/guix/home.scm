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

                  ;; Spelling
                  ;; "emacs-jinx"
                  ;; "enchant"
                  ;; "hunspell"
                  ;; "hunspell-dict-en-us"

                  )))

 ;; (services (list 
 ;;                 (service home-dotfiles-service-type
 ;;                          (home-dotfiles-configuration
 ;;                           (directories '("../../files"))))))

 )
