{ haskellPackages, writers, runCommandLocal, makeWrapper }:

# reference: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/x11/window-managers/xmonad.nix

let
  packages    = self: [ self.termonad self.optparse-applicative ];
  termonadEnv = haskellPackages.ghcWithPackages packages;
  configured  = writers.writeHaskellBin "termonad" {
    ghc       = haskellPackages.ghc;
    libraries = packages haskellPackages;
    ghcArgs   = [];
  } ./termonad.hs;
in
  runCommandLocal "termonad" {
    nativeBuildInputs = [ makeWrapper termonadEnv ];
  } ''
    makeWrapper ${configured}/bin/termonad $out/bin/termonad \
      --set NIX_GHC "${termonadEnv}/bin/ghc"
  ''
