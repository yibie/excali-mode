{
  description = "excali-mode: Excalidraw scenes on Emacs Canvas";

  # No inputs: the package is built from the consumer's Emacs package set.
  outputs = {self}: {
    # The C module must be built against the Emacs that loads it (its
    # emacs-module.h needs canvas_data, i.e. Emacs 32), so it is offered
    # per package set.  Usage, inside emacsWithPackages:
    #   (excali-mode.lib.mkEmacsPackage epkgs)
    # Excalidraw's fonts come too; `.override {excali-fonts = null;}` drops them.
    lib.mkEmacsPackage = epkgs:
      epkgs.callPackage ./nix/excali.nix {
        excali-fonts = epkgs.callPackage ./nix/fonts.nix {};
      };
  };
}
