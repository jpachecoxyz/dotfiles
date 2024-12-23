# This is a Nix script to set up a nix-shell with git and run the required commands.

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [ pkgs.stow pkgs.git];  # Include git and stow to set the dotfiles symlinks

  shellHook = ''

    # Run the NixOS and Home Manager rebuild commands
    echo "Running NixOS and Home Manager switch..."
    sudo nixos-rebuild switch --flake . && home-manager switch --flake .

    # Provide some feedback when done
    echo "Dotfiles setup complete."
    echo ""

    echo "Creating symlinks..."
    cd ~/.dotfiles/
    stow . && echo "Symlinks created..." || echo "Symlinks was not created..."

    cd ~
    mkdir ~/pics
    git clone https://codeberg.org/toru/art pics/art --depth 1
  '';
}
