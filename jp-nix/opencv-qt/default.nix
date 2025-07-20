{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "opencv-qt";
  version = "4.9.0";  # Cambia la versión según lo que necesites

  src = pkgs.fetchFromGitHub {
    owner = "opencv";
    repo = "opencv";
    rev = "4.9.0";  # Mismo que version
    sha256 = "sha256-3qqu4xlRyMbPKHHTIT+iRRGtpFlcv0NU8GNZpgjdi6k=";  # Actualiza según sea necesario
  };

  nativeBuildInputs = with pkgs; [
    cmake
    pkg-config
    python3
  ];

  buildInputs = with pkgs; [
    qt5.full                # Qt support
    gtk3                    # Para highgui si se quiere compatibilidad con GTK también
    libjpeg
    libpng
    libtiff
    libwebp
    ffmpeg
    eigen
    tbb
    openexr
    python3
  ];

  cmakeFlags = [
    "-DWITH_QT=ON"
    "-DWITH_GTK=ON"
    "-DWITH_OPENGL=ON"
    "-DWITH_TBB=ON"
    "-DBUILD_opencv_python3=ON"
    "-DPYTHON3_EXECUTABLE=${pkgs.python3.interpreter}"
    "-DBUILD_TESTS=OFF"
    "-DBUILD_PERF_TESTS=OFF"
    "-DBUILD_EXAMPLES=OFF"
  ];

  enableParallelBuilding = true;

  meta = with pkgs.lib; {
    description = "OpenCV with Qt support";
    homepage = "https://opencv.org/";
    license = licenses.bsd3;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}

