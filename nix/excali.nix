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
  makeFontsConf,
  # Excalidraw's fonts (./fonts.nix); null to fall back to system fonts.
  excali-fonts ? null,
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
  # The sandbox has no fontconfig config or writable cache.
  checkPhase = ''
    runHook preCheck
    export FONTCONFIG_FILE=${makeFontsConf {fontDirectories = [];}}
    export XDG_CACHE_HOME=$TMPDIR
    make $makeFlags test
    runHook postCheck
  '';

  # excali-core.el loads the module from its own directory, and
  # `excali-fonts-directory' defaults to fonts/ beside it.
  installPhase = ''
    runHook preInstall
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install -m644 *.el *.elc $LISPDIR
    install -m755 excali-module${stdenv.hostPlatform.extensions.sharedLibrary} $LISPDIR
    ${lib.optionalString (excali-fonts != null) ''
      # .nosearch keeps fonts/ off load-path; the module skips dotfiles.
      install -d $LISPDIR/fonts
      touch $LISPDIR/fonts/.nosearch
      ln -s ${excali-fonts}/* $LISPDIR/fonts/
    ''}
    runHook postInstall
  '';

  meta = {
    description = "Excalidraw scenes on Emacs Canvas";
    homepage = "https://github.com/yibie/excali-mode";
    license = lib.licenses.gpl3Plus;
  };
}
