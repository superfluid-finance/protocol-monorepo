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
    certora = {
      url = "github:hellwolf/certora.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO use ghc 9.6 when available
    #ghc-wasm.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
    #ghc-wasm.inputs.nixpkgs.follows = "nixpkgs";
    #ghc-wasm.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, foundry, solc, certora } :
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

    # ghc ecosystem
    ghc = pkgs.haskell.compiler.${ghcVer};
    ghcPkgs = pkgs.haskell.packages.${ghcVer};

    # common dev inputs
    commonDevInputs = with pkgs; [
       gnumake
      # for shell script linting
      shellcheck
      # used by some scripts
      jq
    ];

    # solidity dev inputs
    ethDevInputs = with pkgs; [
      foundry-bin
      pkgs.${solcVer}
    ];

    # nodejs ecosystem
    nodeDevInputsWith = nodejs: [
      nodejs
      nodejs.pkgs.yarn
      nodejs.pkgs.nodemon
    ];
    node16DevInputs = nodeDevInputsWith pkgs.nodejs-16_x;
    node18DevInputs = nodeDevInputsWith pkgs.nodejs-18_x;

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
      #ghc-wasm.packages.${system}.default
      ghcPkgs.haskell-language-server
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
    ]

    # certora tooling
    ++ [
      python3
    ] ++ certora.devInputs.${system};

    # mkShell wrapper, to expose additional environment variables
    mkShell = o : pkgs.mkShell ({
      SOLC_PATH = pkgs.lib.getExe pkgs.${solcVer};
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
    devShells.ci-node16 = mkShell {
      buildInputs = commonDevInputs ++ ethDevInputs ++ node16DevInputs;
    };
    devShells.ci-node18 = mkShell {
      buildInputs = commonDevInputs ++ ethDevInputs ++ node18DevInputs;
    };
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
