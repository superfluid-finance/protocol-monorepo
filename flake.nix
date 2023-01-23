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
    specInputs = with pkgs; [
      # for nodejs ecosystem
      yarn
      gnumake
      nodePackages.nodemon
      # for haskell spec
      cabal-install
      haskell.compiler.ghc94
      haskell.packages.ghc94.haskell-language-server
      hlint
      stylish-haskell
      # sage math
      sage
      # testing tooling
      gnuplot
      # yellowpaper pipeline tooling
      haskellPackages.lhs2tex
      python39Packages.pygments
      (texlive.combine {
        inherit (texlive)
        scheme-basic metafont
        collection-latex collection-latexextra
        collection-bibtexextra collection-mathscience
        collection-fontsrecommended collection-fontsextra;
      })
    ];
  in {
    devShells.default = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs;
    };
    devShells.whitehat = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs
        ++ whitehatInputs;
    };
    devShells.spec = with pkgs; mkShell {
      buildInputs = specInputs;
    };
    devShells.full = with pkgs; mkShell {
      buildInputs = minimumEVMDevInputs
      ++ whitehatInputs
      ++ specInputs;
    };
  });
}

