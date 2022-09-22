{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flakeUtils }:
  flakeUtils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShell = with pkgs; mkShell {
      buildInputs = [
        # for nodejs ecosystem
        yarn
        gnumake
        nodePackages.nodemon
        # for haskell spec
        haskell.compiler.ghc924
        haskell-language-server
        gnuplot
        # sage math
        sage
        # yellow-paper pipeline
        haskellPackages.lhs2tex
        python39Packages.pygments
        (texlive.combine {
          inherit (texlive)
          scheme-basic metafont
          collection-latex collection-latexextra collection-bibtexextra;
        })
      ];
    };
  });
}
