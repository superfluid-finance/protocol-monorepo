# Auto Wrap

Auto Wrap is an automated token wrapping system that automatically wraps ERC20 tokens to Super Tokens just in time to keep your streams running.

When your Super Token balance reaches a certain lower threshold, Auto Wrap steps in and wraps enough tokens into the needed Super Token on your behalf to ensure you never run out of balance, as that would make all streams stop.

## Getting Started

### Prerequisites

- Auto Wrap uses [Foundry](https://github.com/gakonst/foundry#installation) as the development framework.
- [Yarn](https://github.com/yarnpkg/yarn) is used as the package manager.

### Environment Variables

- Use .env-template as a template for your .env file

```bash
# .env-template

PRIVATE_KEY=

GOERLI_URL=
MUMBAI_URL=
POLYGON_URL=
BSC_URL=

ETHERSCAN_API_KEY_GOERLI =
ETHERSCAN_API_KEY_MUMBAI =
ETHERSCAN_API_KEY_POLYGON =
ETHERSCAN_API_KEY_BINANCE =
```

#### Run tests

```bash
forge test --vvv
```

#### Deploy

Create a `.env` file using template above and run:

Deployment script will deploy all contracts and verify them on Etherscan.

```bash
npx hardhat deploy --network [NETWORK_NAME]
```
