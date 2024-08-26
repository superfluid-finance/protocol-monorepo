{
  halfBoardModule = {
    dependencies = [
      ../ethereum-contracts
      ../sdk-core
    ];
    outputs = [
      "abi"
      "generate"
    ];
    includedFiles = [
      ./package.json
      ./config
      ./matchstick.yaml
      ./schema.graphql
      ./tsconfig.json
      ./types
      ./src
      ./scripts
      ./tests
    ];
  };
}
