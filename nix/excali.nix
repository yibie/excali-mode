{
  lib,
  src,
  stdenv,
  emacs,
  trivialBuild,
  pkg-config,
  cairo,
  pango,
  zlib,
  fontconfig,
  librsvg,
  gdk-pixbuf,
  libwebp,
}:
trivialBuild {
  pname = "excali";
  version = "0.3.0";
  inherit src;

  nativeBuildInputs = [pkg-config];
  buildInputs = [cairo pango zlib fontconfig librsvg gdk-pixbuf libwebp];

  makeFlags = ["EMACS_MODULE_INCLUDE=${emacs}/include"];

  # The Makefile builds the module, then byte-compiles against it.
  buildPhase = ''
    runHook preBuild
    make $makeFlags all
    runHook postBuild
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    make $makeFlags test
    runHook postCheck
  '';

  # excali-core.el loads the module from its own directory.
  installPhase = ''
    runHook preInstall
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install -m644 *.el *.elc $LISPDIR
    install -m755 excali-module${stdenv.hostPlatform.extensions.sharedLibrary} $LISPDIR
    runHook postInstall
  '';

  meta = {
    description = "Excalidraw scenes on Emacs Canvas";
    homepage = "https://github.com/yibie/excali-mode";
    license = lib.licenses.gpl3Plus;
  };
}
