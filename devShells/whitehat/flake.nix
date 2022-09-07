{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    foundry.url = "github:shazow/foundry.nix";  
    echidna.url = "github:crytic/echidna";
  };

  outputs = { self, nixpkgs, flakeUtils, foundry, echidna }:
  flakeUtils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShell = with pkgs; mkShell {
      buildInputs = [
        # for nodejs ecosystem
        yarn
        nodejs-16_x
        # for solidity development
        foundry.defaultPackage.${system}
        slither-analyzer
        echidna.defaultPackage.${system}
      ];
    };
  });
}
