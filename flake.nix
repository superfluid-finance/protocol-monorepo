{
  description = "Overlay for working with Superfluid protocol monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    foundry.url = "github:shazow/foundry.nix/monthly";
  };

  outputs = { self, nixpkgs, flakeUtils, foundry } :
  flakeUtils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; };
    # minimem development shell
    minimumEVMDevInputs = with pkgs; [
      # for nodejs ecosystem
      yarn
      nodejs-16_x
      # for solidity development
      foundry.defaultPackage.${system}
    ];
    # additional tooling for whitehat hackers
    whitehatInputs = with pkgs; [
      slither-analyzer
      echidna
    ];
    # for developing specification
    ghcVer = "ghc944";
    ghc = pkgs.haskell.compiler.${ghcVer};
    ghcPackages = pkgs.haskell.packages.${ghcVer};
    specInputs = with pkgs; [
      # for nodejs ecosystem
      yarn
      gnumake
      nodePackages.nodemon
      # for haskell spec
      cabal-install
      ghc
      ghcPackages.haskell-language-server
      hlint
      stylish-haskell
      # sage math
      sage
      # testing tooling
      gnuplot
      # yellowpaper pipeline tooling
      ghcPackages.lhs2tex
      python39Packages.pygments
      (texlive.combine {
        inherit (texlive)
        scheme-basic metafont
        collection-latex collection-latexextra
        collection-bibtexextra collection-mathscience
        collection-fontsrecommended collection-fontsextra;
      })
    ];
    ci-spec = ghcVer : with pkgs; mkShell {
      buildInputs = [
        gnumake
        cabal-install
        haskell.compiler.${ghcVer}
        hlint
      ];
    };
  in {
    devShells.default = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs;
    };
    devShells.whitehat = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs
        ++ whitehatInputs;
    };
    devShells.spec = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs
        ++ specInputs;
    };
    devShells.full = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs
      ++ whitehatInputs
      ++ specInputs;
    };
    devShells.ci-spec-ghc925 = ci-spec "ghc925";
    devShells.ci-spec-ghc944 = ci-spec "ghc944";
    devShells.ci-hot-fuzz = with pkgs; mkShell {
        buildInputs = [ echidna ];
    };
  });
}
