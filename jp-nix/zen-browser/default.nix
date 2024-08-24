# https://github.com/zen-browser/desktop/releases/download/1.0.0-a.22/zen.linux-specific.tar.bz2

{ stdenv, lib, fetchurl, }:

stdenv.mkDerivation rec {
  pname = "zen-browser";
  version = "1.0.0-a.22";

  src = fetchurl {
    url = "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-specific.tar.bz2";
    sha256 = "9BzyZxy7FRs2zAO9j6WK/0ltvyyUJdDMMLnPdIU5AL0=";
  };

  # src = fetchFromGitHub {
  #   owner = "zen-browser";
  #   repo = "desktop";
  #   rev = "v${version}";
  #   sha256 = "0000000000000000000000000000000000000000000000000000";
  # };

  dontBuild = true;

  unpackPhase = ''
    tar xjf $src
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp zen/zen-bin  $out/bin/
    # cp $src $out/bin/
  '';

  # propagatedBuildInputs = [ runTimePackage ];

  meta = with lib; {
    description = "Experience tranquillity while browsing the web without people tracking you!";
    homepage = "https://www.zen-browser.app/";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
