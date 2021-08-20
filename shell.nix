{ pkgs ? import <nixpkgs> {}, compiler ? null }:
let
  haskellPackages = (if   isNull compiler
                     then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override {
    overrides = self: super: {
    };
  };
in
  (pkgs.callPackage ./default.nix {
    haskellPackages = haskellPackages;
  }).overrideDerivation (old: {
    shellHook = ''
      ghci termonad.hs
      exit 0
    '';
  })
