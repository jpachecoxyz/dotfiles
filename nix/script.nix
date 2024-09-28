{ pkgs }:

pkgs.writeShellScriptBin "cowcat" ''
    echo "Hello world!" | ${pkgs.cowsay}/bin/cowsay | ${pkgs.lolcat}/bin/lolcat
''
