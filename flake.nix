{
  description = "termonad customs";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url    = "github:numtide/devshell";
  };

  outputs = { self, ... }@inputs: (inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      inputs.devshell.flakeModule
    ];
    systems   = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    perSystem = { pkgs, system, ... }: rec {
      packages.default =
        let
          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {
              # Fix build failure with latest haskell-gi.
              # Since haskell-gi changed its packaging policy, we must explicitly use 'gi-gtk3' and other versioned packages instead of the generic ones.
              # See: https://github.com/NixOS/nixpkgs/issues/446880
              termonad = (super.callHackageDirect {
                pkg    = "termonad";
                ver    = "4.6.0.0";
                sha256 = "GErZ6pJ9MmxVI+zFbTmn1uTJMer1NNMUBuexHKnUQO8=";
              } {
                vte_291 = pkgs.vte;
                gi-gtk  = super.gi-gtk3;
                gi-gdk  = super.gi-gdk3;
              }).overrideDerivation (old: {
                prePatch = ''
                  sed -i '
                    s/, gi-gtk >= 3.0.24$/, gi-gtk3/
                    s/, gi-gdk$/, gi-gdk3/
                  ' termonad.cabal
                '';
              });
            };
          };
        in pkgs.callPackage ./default.nix { inherit haskellPackages; };
      devshells.default = {
        packages = packages.default.nativeBuildInputs ++ [ pkgs.hlint ];
        devshell.startup.ghci.text  = ''
          ghci termonad.hs
          exit 0
        '';
      };
    };
  }) // {
    name         = "termonad";
    nixosModules = rec {
      addpkg = { pkgs, ... }: {
        nixpkgs.config = {
          packageOverrides = oldpkgs: let newpkgs = oldpkgs.pkgs; in {
            "${self.name}" = self.packages."${pkgs.stdenv.hostPlatform.system}".default;
          };
        };
      };

      install = { pkgs, ... }: (addpkg { inherit pkgs; }) // {
        environment.systemPackages = [ pkgs."${self.name}" ];
      };
    };
  };
}
