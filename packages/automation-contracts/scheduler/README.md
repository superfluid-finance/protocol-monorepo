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

GOERLI_PRIVATE_KEY=
MUMBAI_PRIVATE_KEY=
POLYGON_PRIVATE_KEY=
BSC_PRIVATE_KEY=

GOERLI_URL=
MUMBAI_URL=
POLYGON_URL=
BSC_URL=

ETHERSCAN_API_KEY=
```

#### Run tests

```bash
forge test --vvv
```

#### Deploy

Create a `.env` file using example above and run:

Deployment script will deploy all contracts and verify them on Etherscan.

```bash
npx hardhat deploy --network <network>
```


#### Deployed Contracts

#### Testnets
|          | FlowScheduler                                                                                                                        | VestingScheduler                                                                                                                     |
|----------|--------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| Goerli   | [0xA6134E107FCAEAab6140CB8fb92Dbf5bd9EF6C86](https://goerli.etherscan.io/address/0xA6134E107FCAEAab6140CB8fb92Dbf5bd9EF6C86#code)    | [0xb61f8A2CbBd1be5FE437509b51dc2B9BA710c47b](https://goerli.etherscan.io/address/0xb61f8A2CbBd1be5FE437509b51dc2B9BA710c47b#code)    |
| Mumbai   | [0xF18825d412C061aEfEFB4dF46a1c077636dA50bf](https://mumbai.polygonscan.com/address/0xF18825d412C061aEfEFB4dF46a1c077636dA50bf#code) | [0xD4fCe2a08fd2fA7495BEd900F2fb6c6deDBf8632](https://mumbai.polygonscan.com/address/0xD4fCe2a08fd2fA7495BEd900F2fb6c6deDBf8632#code) |

#### Mainnets
|         | FlowScheduler                                                                                                                 | VestingScheduler                                                                                                              |
|---------|-------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| Polygon | [0x47D34512492D95A3531A628e5B85e32fAFaC1b42](https://polygonscan.com/address/0x47D34512492D95A3531A628e5B85e32fAFaC1b42#code) | [0xF9B3b4c23d08ebcBb8A70F5C7471E3Edd3ddF210](https://polygonscan.com/address/0xF9B3b4c23d08ebcBb8A70F5C7471E3Edd3ddF210#code) |
| BSC     | [0x1D65c6d3AD39d454Ea8F682c49aE7744706eA96d](https://bscscan.com/address/0x1D65c6d3AD39d454Ea8F682c49aE7744706eA96d#code)     | [0x4f268bfB109439D7c23A903c237cdBEbd7E987a1](https://bscscan.com/address/0x4f268bfB109439D7c23A903c237cdBEbd7E987a1#code)     |
