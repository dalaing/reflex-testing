{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} : 
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ../reflex-platform {};
  drv = import ./. { inherit reflex-platform compiler; };
in
  if pkgs.lib.inNixShell then drv.env else drv
