{ stdenv, fetchFromGitHub, autoconf, automake, makeWrapper, gcc, gnumake, pkg-config, tree-sitter, mailutils, libpng, libjpeg, librsvg, libtiff, xorg, giflib, cairo, pango, gnutls, libxml2, jansson, libgcc, libgccjit, zlib, ncurses, texinfo, libtool, perl }:

stdenv.mkDerivation rec {
  pname = "emacs";
  version = "master";

  src = fetchFromGitHub {
    owner = "emacs-mirror";
    repo = "emacs";
    rev = "master";
    sha256 = "sha256-s5vg4QjR07QLqlucx4ZaT1PDgIp3o5uB0x7TFhV2F1Y="; # Update this hash
  };

  nativeBuildInputs = [ autoconf automake pkg-config libtool];
  buildInputs = [
    gcc gnumake tree-sitter mailutils libpng libjpeg librsvg libtiff
    xorg.libX11 xorg.libXpm xorg.libXaw giflib cairo pango
    gnutls libxml2 jansson libgccjit libgcc zlib ncurses texinfo perl
  ];

  # unpackPhase = "true";
  configureFlags = [
    "--without-libgccjit" # Disable libgccjit support
  ];

  configurePhase = ''
    # cd $src
    # export LIBRARY_PATH=$LIBRARY_PATH:${libgccjit}/lib
    # export CPATH=$CPATH:${libgccjit}/include
    ./autogen.sh

    # Debugging output
    echo "Library paths:"
    echo $LIBRARY_PATH

    echo "Include paths:"
    echo $CPATH

    ./configure \
      --with-tree-sitter \
      --with-native-compilation \
      --with-pop
    echo "configure CORRECT"
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out
    make install DESTDIR=$out
  '';

  # Wrap the Emacs binary if needed
  # postInstall = ''
  #   wrapProgram $out/bin/emacs \
  #     --prefix PATH : ${lib.makeBinPath [ git ]}
  # '';

  meta = with stdenv.lib; {
    description = "The GNU Emacs editor";
    # license = licenses.gpl3Plus;
    # platforms = platforms.linux;
  };
}

