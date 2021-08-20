{ haskellPackages, writers, runCommandLocal, makeWrapper }:

# reference: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/x11/window-managers/xmonad.nix

let
  cmdName     = "termonad";
  packageName = cmdName;
  packages    = self: [ self.termonad self.optparse-applicative ];
  termonadEnv = haskellPackages.ghcWithPackages packages;
  configured  = writers.writeHaskellBin cmdName {
    ghc       = haskellPackages.ghc;
    libraries = packages haskellPackages;
    ghcArgs   = [];
  } ./termonad.hs;
in
  runCommandLocal packageName {
    nativeBuildInputs = [ makeWrapper termonadEnv ];
  } ''
    makeWrapper ${configured}/bin/${cmdName} $out/bin/${cmdName} \
      --set NIX_GHC "${termonadEnv}/bin/ghc"
  ''
