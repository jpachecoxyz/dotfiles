{ stdenv, fetchFromGitHub, libpcap, ncurses, lib, dialog }:

stdenv.mkDerivation rec {
  pname = "whdd";
  version = "master";

  src = fetchFromGitHub {
    owner = "whdd";
    repo = "whdd";
    rev = "master";
    sha256 = "sha256-jhNLQVls7mfNuMw0/Rc2bPtQjpV0qiOlvyF9P+pnOMg=";
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
