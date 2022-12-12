# Scheduler

Set of contracts that allow to schedule a Superfluid tasks related to be executed in the future.

## Flow Scheduler

If you have an intended end date and/or start date for a stream, instead of having to manually trigger the action, you could use the Stream Scheduler to automatically trigger the action at the desired time. This is especially useful in the case of streaming payroll, subscriptions and token vesting.

## Vesting Scheduler

The Vesting Scheduler allows you to schedule the vesting of tokens to a receiver account. The Vesting Scheduler does not hold the tokens, rather it simply uses permissions to move them for you


## Getting Started

### Prerequisites

- Scheduler uses [Foundry](https://github.com/gakonst/foundry#installation) as the development framework.
- [Yarn](https://github.com/yarnpkg/yarn) is used as the package manager.

### Environment Variables

- Use `.env-example` as a template for your .env file

```bash
# .env-example

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

Create a `.env` file using example above and run:

Deployment script will deploy all contracts and verify them on Etherscan.

```bash
npx hardhat run --network <network> deploy/deploy.js
```
