{
  description = "Overlay for working with Superfluid protocol monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry = {
      url = "github:shazow/foundry.nix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, foundry, solc } :
  flake-utils.lib.eachDefaultSystem (system:
  let
    solcVer = "solc_0_8_19";
    ghcVer = "ghc944";

    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        foundry.overlay
        solc.overlay
      ];
    };

    # minimem development shell
    minimumEVMDevInputs = with pkgs; [
      # for nodejs ecosystem
      yarn
      nodejs-18_x
      # for solidity development
      foundry-bin
      pkgs.${solcVer}
      # for shell script linting
      shellcheck
      # used by some scripts
      jq
    ];
    # additional tooling for whitehat hackers
    whitehatInputs = with pkgs; [
      slither-analyzer
      echidna
    ];
    # for developing specification
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

    mkShell = o : pkgs.mkShell ({
      SOLC_PATH = pkgs.lib.getExe pkgs.${solcVer};
    } // o);

    ci-spec-with-ghc = ghcVer : mkShell {
      buildInputs = with pkgs; [
        gnumake
        cabal-install
        haskell.compiler.${ghcVer}
        hlint
      ];
    };
  in {
    # local development shells
    devShells.default = mkShell {
      buildInputs = minimumEVMDevInputs;
    };
    devShells.whitehat = mkShell {
      buildInputs = minimumEVMDevInputs
        ++ whitehatInputs;
    };
    devShells.spec = mkShell {
      buildInputs = minimumEVMDevInputs
        ++ specInputs;
    };
    devShells.full = mkShell {
      buildInputs = minimumEVMDevInputs
        ++ whitehatInputs
        ++ specInputs;
    };
    # CI shells
    devShells.ci-spec-ghc925 = ci-spec-with-ghc "ghc925";
    devShells.ci-spec-ghc944 = ci-spec-with-ghc "ghc944";
    devShells.ci-hot-fuzz = mkShell {
      buildInputs = with pkgs; [
        slither-analyzer
        echidna
      ];
    };
  });
}
