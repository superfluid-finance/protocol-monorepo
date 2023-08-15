{
  description = "Overlay for working with Superfluid protocol monorepo";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    foundry = {
      url = "github:shazow/foundry.nix/monthly";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, foundry, solc } :
  flake-utils.lib.eachDefaultSystem (system:
  let
    minDevSolcVer = "solc_0_8_11"; # minimum solidity version used for external development
    solcVer = "solc_0_8_19";
    ghcVer92 = "ghc928";
    ghcVer94 = "ghc945";

    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        foundry.overlay
        solc.overlay
      ];
    };

    # ghc ecosystem
    ghc = pkgs.haskell.compiler.${ghcVer94};
    ghcPkgs = pkgs.haskell.packages.${ghcVer94};

    # common dev inputs
    commonDevInputs = with pkgs; [
      gnumake
      # for shell script linting
      shellcheck
      # used by some scripts
      jq
      # test utilities
      lcov
      actionlint
    ];

    # solidity dev inputs
    ethDevInputs = with pkgs; [
      foundry-bin
      pkgs.${minDevSolcVer}
      pkgs.${solcVer}
      (solc.mkDefault pkgs pkgs.${solcVer})
    ];

    # nodejs ecosystem
    nodeDevInputsWith = nodejs: [
      nodejs
      nodejs.pkgs.yarn
      nodejs.pkgs.nodemon
    ];
    node18DevInputs = nodeDevInputsWith pkgs.nodejs_18;
    node20DevInputs = nodeDevInputsWith pkgs.nodejs_20;

    # minimem development shell
    minimumDevInputs = commonDevInputs ++ ethDevInputs ++ node18DevInputs;

    # additional tooling for whitehat hackers
    whitehatInputs = with pkgs; [
      slither-analyzer
      echidna
    ];

    # spec developing specification
    specInputs = with pkgs; [
      # for nodejs ecosystem
      yarn
      gnumake
      # for haskell spec
      cabal-install
      ghc
      hlint
      stylish-haskell
      # sage math
      sage
      # testing tooling
      gnuplot
      # yellowpaper pipeline tooling
      ghcPkgs.lhs2tex
      python39Packages.pygments
      (texlive.combine {
        inherit (texlive)
        scheme-basic metafont
        collection-latex collection-latexextra
        collection-bibtexextra collection-mathscience
        collection-fontsrecommended collection-fontsextra;
      })
    ];

    # mkShell wrapper, to expose additional environment variables
    mkShell = o : pkgs.mkShell ({
      SOLC = pkgs.lib.getExe pkgs.${solcVer};
    } // o);

    # ci-spec-with-ghc
    ci-spec-with-ghc = ghcVer : mkShell {
      buildInputs = with pkgs; [
        cabal-install
        haskell.compiler.${ghcVer}
        hlint
      ];
    };
  in {
    # local development shells
    devShells.default = mkShell {
      buildInputs = minimumDevInputs;
    };
    devShells.whitehat = mkShell {
      buildInputs = minimumDevInputs
        ++ whitehatInputs;
    };
    devShells.spec = mkShell {
      buildInputs = minimumDevInputs
        ++ specInputs;
    };
    devShells.full = mkShell {
      buildInputs = minimumDevInputs
        ++ whitehatInputs
        ++ specInputs;
    };
    # CI shells
    devShells.ci-node18 = mkShell {
      buildInputs = commonDevInputs ++ ethDevInputs ++ node18DevInputs;
    };
    devShells.ci-node20 = mkShell {
      buildInputs = commonDevInputs ++ ethDevInputs ++ node20DevInputs;
    };
    devShells.ci-spec-ghc92 = ci-spec-with-ghc ghcVer92;
    devShells.ci-spec-ghc94 = ci-spec-with-ghc ghcVer94;
    devShells.ci-hot-fuzz = mkShell {
      buildInputs = with pkgs; commonDevInputs ++ ethDevInputs ++ [
        slither-analyzer
        echidna
      ];
    };
  });
}
