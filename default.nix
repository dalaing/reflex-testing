{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghc"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = reflex-platform.${compiler}.callPackage ./reflex-testing.nix {};
in
  drv
