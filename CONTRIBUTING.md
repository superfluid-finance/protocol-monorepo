# Contributing

Interested in contributing, or just troubleshooting? Great! Let's get this party started.

Before interacting with the Superfluid community, please read and understand our [Code of Conduct](code_of_conduct.md).

## Local Development

### Setup Tooling

At minimum, you will need to have these available in your development environment:

- yarn, sufficiently recent version, the actual yarn version is locked in yarnc.
- nodejs 16.x.

**More Options Using Nix**

You may also use [nix package manager](https://nixos.org/download.html) to get a reproducible, declarative and reliable development environment.

Development shells options are available under `devShells` folder ([nix flakes](https://nixos.wiki/wiki/Flakes) required):

- minimum, minimum development environment for building monorepo.
- whitehat, additional ethereum development toolings including slither, echidna, etc.
- spec, for developing of Superfluid haskell spec.

To use them:

- `nix develop path:flakes/minimum` or `npm run shell`
- `nix develop path:flakes/whitehat` or `npm run shell:whitehat`
- `nix develop path:flakes/spec` or `npm run shell:spec`

### Installing Dependencies

Before you do anything, you should run `yarn install && yarn build` in the root directory of your local-copy of the protocol-monorepo to install and build the necessary dependencies.

```bash
cd protocol-monorepo
yarn install && yarn build
```

You'll also want to upgrade your Superfluid App to the canary, so you have the most recent changes.

```bash
cd superfluid-app # wherever your superfluid-app happens to be
yarn upgrade @superfluid-finance/ethereum-contracts@dev @superfluid-finance/sdk-core@dev
```

### Copy and Watch

Now you are ready. If you're editing the Superfluid contracts, start the auto-compiler:

```bash
cd packages/ethereum-contracts
npx truffle watch
```

Then, copy-and-watch the changes into your Superfluid App.

```bash
nodemon --watch ../path/to/superfluid/packages -ext js,ts,tsx,sol --exec rsync --archive --delete ../path/to/superfluid/packages ./node_modules/@superfluid-finance/
```

For example, if your Superfluid App and protocol-monorepo are located in the same directory:

```bash
📦projects
 ┣ 📂superfluid-app
 ┗ 📂protocol-monorepo
   ┗ 📂packages

cd superfluid-app
nodemon --watch ../protocol-monorepo/packages --ext js,ts,tsx,sol --exec rsync --archive --delete ../protocol-monorepo/packages/ ./node_modules/@superfluid-finance/
```

See [examples/](examples/) if you want to see more examples of this method.

## Code Coverage

We are using `solidity-coverage`. Use this command to run the coverage tests in the contracts package:

```bash
truffle run test-coverage
```

> Note: This step is not integrated with the unit test because of the time it consumes to execute.

## Testing

See the individual packages for specific details on testing or run all the tests from the root `npm run test` (smart contract tests can run over an hour).

## Linting

We are using [eslint](https://eslint.org/) for Javascript and [solhint](https://protofire.github.io/solhint/) for Solidity.

## Releases

### Master `@latest`

To publish a new version of Superfluid to NPM run the following command:

```bash
yarn lerna:version
```

Packages are versioned independently, so you can choose which packages to release, and commit only those changes. Now its time to publish:

```bash
scripts/publish-master.sh
```

There should already be a draft release created in Github for each package. Add in your changes and publish it.

### Canary `@dev`

Whenever anything is merged to `dev`, new packages are automatically published to our [Github packages](https://github.com/orgs/superfluid-finance/packages?repo_name=protocol-monorepo). A Github release should not be created.

Canary packages can be install using the `@dev` tag

```bash
yarn install @superfluid-finance/ethereum-contracts@dev

# or specify the version
yarn install @superfluid-finance/ethereum-contracts@0.2.4-dev.265
```

### Pull Request `@PRxxx`

Pull request packages are automatically published to our [Github packages](https://github.com/orgs/superfluid-finance/packages?repo_name=protocol-monorepo). A Github release should not be created.

See the bot message in the PR for how to install these packages.

## Known Local Development Issues

### `node-jq` error when Node.js is installed through Snap Store
#### Problem
Error message when running `yarn install`:
```
error /protocol-monorepo/node_modules/node-jq: Command failed.
Exit code: 243
Command: npm run install-binary
Arguments:
```
* [Relevant StackOverflow question](https://stackoverflow.com/questions/67475457/why-cant-i-just-run-npm-install-via-a-child-process-exec-call-npm-exit-243-wit)

#### Solutions
* Re-install Node.js through other means than Snap Store.
