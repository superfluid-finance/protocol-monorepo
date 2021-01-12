<h1 align="center">Welcome to superfluid-monorepo 👋</h1>
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/ethereum-contracts" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/ethereum-contracts.svg">
  </a>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/status/" target="_blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>

> Contracts and resources for the Superfluid Protocol

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

The Superfluid Protocol is a framework that realizes the real-time finance vision
where user accounts are connected together, and transactions can happen between
user accounts instantaneously as a result.

This repository implements the superfluid protocol as Ethereum contracts. It also
contains a Javascript SDK for developing Web3 applications using the superfluid
protocol.

For technical document, references and tutorials, etc, refer to the
[docs site](http://docs.superfluid.finance/).

## Packages

#### `@superfluid-finance/js-sdk`

To build with Superfluid, you can use the Javascript SDK package.

#### `@superfluid-finance/ethereum-contracts`

If you're interest in peeking under the hood, then check out the contracts package.

## Examples

See `/examples` for some Superfluid app examples.

## Contributing

Contributions, issues, and feature suggestions are welcome!

### Installation

Interested in contributing, or just troubleshooting? Great! Let's get this party started.

```bash
git clone https://github.com/superfluid-finance/ethereum-contracts
cd ethereum-contracts
yarn install
yarn build
```

Now you are ready to make changes, run tests, and troubleshoot. If you want to see changes reflected in your own project, we use `rsync` to keep things up to date. I wouldn't waste time trying `npm link` or `yarn link`.

If you're editing contracts, start auto-compiling them in `/packages/ethereum-contracts`.

```bash
truffle watch
```

Now in your own project, lets auto-sync the superfluid packages. See `/examples` for example projects using this method.

```bash
nodemon --watch ../path/to/superfluid/packages -ext js,ts,tsx,sol --exec rsync -rtvu --delete ../path/to/superfluid/packages ./node_modules/@superfluid-finance/
```

### Testing

See the individual packages for more specific details about testing.

### Linting

Javascript is linted using [eslint](https://eslint.org/).

Solidity is linted using [solhint](https://protofire.github.io/solhint/)

### Code Coverage

To run the coverage tests please use:

```
$ truffle run test-coverage
```

This step is not integrated with the unit test because of the time it consumes to execute.
