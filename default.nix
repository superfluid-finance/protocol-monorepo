{
  halfBoardModule = {
    includedFiles = [
      # development tooling defined in nix
      ./flake.nix
      ./flake.lock
      # managing the npm dependencies with yarn
      ./package.json
      ./.yarnrc.yml
      ./.yarn/releases
      ./yarn.lock
    ];
  };
}
