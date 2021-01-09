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

Interested in contributing, or just troubleshooting? Great! Let's get the party started using yarn.

```bash
git clone https://github.com/superfluid-finance/ethereum-contracts
cd ethereum-contracts
yarn install
```

Now you are ready to make changes, run tests, and maybe even `yarn link` your way to a solution :)

### Testing

Superfluid relies on a persistent 1820 registry contract, so you'll need to make sure its deployed before you can run tests. For this example, we'll deploy the 1820 contract on ganache. (read more about [EIP 1820 Pseudo-introspection Registry Contract](https://eips.ethereum.org/EIPS/eip-1820))

```bash
# Start Ganache on 127.0.0.1:8545
ganache-cli

# In a new terminal
cd packages/ethereum-contracts
yarn build
npx truffle exec scripts/deploy-erc1820.js --network ganache

# Now run your tests
yarn test
```

See the individual packages for more specific details about testing.

### Linting

Javascripts are linted using [eslint](https://eslint.org/).

Solidity are linted using [solhint](https://protofire.github.io/solhint/)

### Code Coverage

To run the coverage tests please use:

```
$ truffle run test-coverage
```

This step is not integrated with the unit test because of the time it consumes to execute.
