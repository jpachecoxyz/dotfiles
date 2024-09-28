{
  description = "A screencast recorder tool";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        my-name = "screencast";
        my-buildInputs = with pkgs; [ tofi wl-clipboard slurp wf-recorder ];
        shell-screencast = (pkgs.writeScriptBin my-name (builtins.readFile ./screencast.sh)).overrideAttrs(old: {
          buildCommand = "${old.buildCommand}\n patchShebangs $out";
        });
      in rec {
        defaultPackage = packages.shell-screencast;
        packages.shell-screencast = pkgs.symlinkJoin {
          name = my-name;
          paths = [ shell-screencast ] ++ my-buildInputs;
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = "wrapProgram $out/bin/${my-name} --prefix PATH : $out/bin";
        };
      }
    );
}
