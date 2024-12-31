(define-module (lazygit)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang) ; Import Go packages
  #:use-module (guix licenses))

(define-public lazygit
  (package
   (name "lazygit")
   (version "0.44.1") ; Update to the latest version
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/jesseduffield/lazygit")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0p9w00rnhdmig3qkv1j4sm7izy6ljxkas0s6p75v3w3a0hr4zzh4")))) ; Update hash
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/jesseduffield/lazygit"
      #:phases
      (modify-phases %standard-phases
					 (add-after 'unpack 'use-vendored-dependencies
								(lambda _
								  (setenv "GOFLAGS" "-mod=vendor")
								  #t)))))
   (inputs
    `(("go" ,go-1.20))) ; Adjust Go version as needed
   (home-page "https://github.com/jesseduffield/lazygit")
   (synopsis "Simple terminal UI for git commands")
   (description
    "Lazygit is a simple terminal UI for git commands, designed to make it easier to
perform common git actions.")
   (license expat)))
