{ reflex-platform ? import ./reflex-platform.nix
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = reflex-platform.ghc.callPackage ./reflex-testing.nix {};
in
  drv
