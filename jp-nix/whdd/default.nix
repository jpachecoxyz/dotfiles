{ stdenv, fetchFromGitHub, libpcap, ncurses, lib, dialog }:

stdenv.mkDerivation rec {
  pname = "whdd";
  version = "master";

  src = fetchFromGitHub {
    owner = "whdd";
    repo = "whdd";
    rev = "master";
    sha256 = "sha256-l13QGahjGxOxHNwXIFcwgzT/jek7QsqANID7x0ZtLf8=";
  };

  nativeBuildInputs = [ libpcap ncurses dialog];

  buildInputs = [ ncurses ];

  patchPhase = ''
    sed -i 's/^LIBS =/LIBS = -lncursesw/' Makefile
  '';

  buildPhase = ''
    export LDFLAGS="-lncursesw"
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp whdd $out/bin/
  '';

  meta = with lib; {
    description = "A simple GUI tool to show and manipulate disks in Linux.";
    homepage = "https://github.com/whdd/whdd";
    license = licenses.gpl2Plus;
    maintainers = [];
  };
}
