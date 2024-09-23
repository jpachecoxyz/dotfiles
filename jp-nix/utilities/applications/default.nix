{ lib, pkgs, ... }:

{
  home.activation.tangleOrg = lib.mkAfter ''
    echo "Tangling applications.org file..."
    ${pkgs.emacs}/bin/emacs --batch /home/javier/.dotfiles/.emacs.d/lisp/applications.org \
      --eval "(require 'org)" \
      --eval "(org-babel-tangle)"
  '';
}
