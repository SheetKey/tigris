{
  description = "A Haskell project template.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";

    rhine-unstable = {
      url = "github:turion/rhine";
      flake = false;
    };
    
    rhine-pinned = {
      url = "github:turion/rhine/ed6476e4040417ab97ab3896dc8cdc312607c3d9";
      flake = false;
    };

    vox-hs-in = {
      url = "github:SheetKey/vox-hs";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rhine-unstable, vox-hs-in, rhine-pinned }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend (final: prev: {
          config = prev.config // { allowBroken = true; };
          haskellPackages = prev.haskellPackages.extend (hfinal: hprev: {
            vox-hs = vox-hs-in.packages.${system}.vox-hs;
          });
        });
        
        # haskellPackages = pkgs.haskellPackages;

        packageName = "tigris";

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.${packageName} = # (ref:haskell-package-def)
          pkgs.haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            # Ex with gi-gtk-declarative:
            # If version is broken then:
            # gi-gtk-declarative = jailbreakUnbreak haskeppPackages.gi-gtk-declarative;
            # or if tests failing: 
            # gi-gtk-declarative = pkgs.haskell.lib.dontCheck haskellPackages.gi-gtk-declarative;

            # rhine = jailbreakUnbreak (pkgs.haskellPackages.callCabal2nix "rhine" (rhine-unstable + "/rhine") { });
            rhine = jailbreakUnbreak pkgs.haskellPackages.rhine;
            # rhine = jailbreakUnbreak (pkgs.haskellPackages.callCabal2nix "rhine" (rhine-pinned + "/rhine") {});
            vox-hs = jailbreakUnbreak pkgs.haskellPackages.vox-hs;
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.implicit-hie
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
