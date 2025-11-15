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
              termonad = super.termonad.overrideAttrs (old: {
                meta = old.meta // { broken = false; };
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
            "${self.name}" = self.packages."${pkgs.system}".default;
          };
        };
      };

      install = { pkgs, ... }: (addpkg { inherit pkgs; }) // {
        environment.systemPackages = [ pkgs."${self.name}" ];
      };
    };
  };
}
