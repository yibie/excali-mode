# Excalidraw's fonts, as `make fonts' prepares them (fonts/README): each
# family's WOFF2 unicode-range subsets converted to TTF and merged into
# one file, plus the complete Xiaolai TTF.  TTF because FreeType reads
# WOFF2 only when built with Brotli.  All SIL OFL 1.1 except Comic Shanns
# (MIT).
{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchurl,
  woff2,
  python3,
}: let
  # fonts/excali-fetch-fonts.el's `excali-fonts--families'.
  families = ["Excalifont" "Nunito" "Lilita" "ComicShanns" "Liberation" "Cascadia" "Virgil"];
  # Merged files whose internal family name is not the one
  # src/excali-text.c asks for ("Nunito ExtraLight", "Comic Shanns
  # Regular"), so fontconfig would fall back to a system font.
  renames = {
    Nunito = "Nunito";
    ComicShanns = "Comic Shanns";
  };
  python = python3.withPackages (ps: [ps.fonttools]);
in
  stdenvNoCC.mkDerivation {
    pname = "excali-fonts";
    version = "0-unstable-2026-11-10";

    src = fetchFromGitHub {
      owner = "excalidraw";
      repo = "excalidraw";
      rev = "701b901a0f1ba3d0b390d9cdf63d112e25abac4b";
      sparseCheckout = ["packages/excalidraw/fonts"];
      hash = "sha256-0eHYBRDxjGdA1anf9CIREro3CII213lZwNVdUe2H39I=";
    };

    xiaolai = fetchurl {
      url = "https://github.com/lxgw/kose-font/releases/download/v3.126/Xiaolai-Regular.ttf";
      hash = "sha256-4vaNrw5yd3qM9YvIPeG5hjSyUeU33b/KJLCuUNGALaI=";
    };

    nativeBuildInputs = [woff2 python];

    buildPhase = ''
      runHook preBuild
      for family in ${lib.escapeShellArgs families}; do
        mkdir -p "out/$family"
        for woff in packages/excalidraw/fonts/"$family"/*.woff2; do
          woff2_decompress "$woff"
        done
        ttfs=(packages/excalidraw/fonts/"$family"/*.ttf)
        if (( ''${#ttfs[@]} > 1 )); then
          pyftmerge --output-file="out/$family/$family-Regular.ttf" "''${ttfs[@]}"
        else
          cp "''${ttfs[0]}" "out/$family/"
        fi
      done
      ${lib.concatStrings (lib.mapAttrsToList (dir: family: ''
          python ${./set-font-family.py} out/${dir}/${dir}-Regular.ttf ${lib.escapeShellArg family}
        '')
        renames)}
      install -Dm644 "$xiaolai" out/Xiaolai/Xiaolai-Regular.ttf
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cp -r out "$out"
      runHook postInstall
    '';

    meta = {
      description = "Excalidraw's fonts, for excali-mode";
      license = with lib.licenses; [ofl mit];
    };
  }
