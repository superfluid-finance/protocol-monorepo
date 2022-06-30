### Welcome to the Money Router - a basic example to get you started with the Superfluid Constant Flow Agreement

1) Run `yarn` to install 
2) `yarn compile` to compile contracts
3) `yarn test` to run the test suite.
4) Create a .env file based on .env.example. Note: if you're using a different network from Goerli, make sure you specify that
5) Make sure that you've set `goerli` (or your other preferred network) as the default network and ensure that it is added to your list of accounts.
6) Run scripts by using `yarn SCRIPT_NAME`

Note: 
1) Before calling any of the create or update flow into contract functions, you'll first need to run the acl approval script
2) Before sending a lump sum into the contract, you'll need to run the token approval script.