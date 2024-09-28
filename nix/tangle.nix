{ pkgs, lib, ... }:

let
  # Define the path to the Emacs org file to tangle
  orgFilePath = "${pkgs.writeTextFile {
    name = "applications-org-path";
    text = "~/.dotfiles/.emacs.d/lisp/applications.org";
  }}";

in
{
  # Step 1: Create the tangle script
  home.file.".local/bin/tangle-org.sh" = {
    text = ''
      #!/bin/sh
      ${pkgs.emacs}/bin/emacs --batch \
        --eval "(require 'org)" \
        --eval "(progn (find-file \\"${orgFilePath}\\") (org-babel-tangle))"
    '';
    executable = true;
  };

  # Step 2: Run the script during home-manager activation
  home.activation.tangleOrgFile = lib.mkAfter ''
    # Run the tangle script
    ~/.local/bin/tangle-org.sh
  '';
}

