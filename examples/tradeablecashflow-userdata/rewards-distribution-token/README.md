Rewards Distribution Token
==========================

A ERC20 token that tokenizes units in the [Instant Distribution Agreements](https://docs.superfluid.finance/superfluid/protocol-tutorials/perform-an-instant-distribution/).

All holders of the rights tokens can receive cash token rewards in one distribution transaction in two ways:

- If the subscription to the token is approved by its holder, then the distribution is available the moment distribution is done,
- if not, then the pending distribution could be claimed by its holder any time in a separated transaction.

# Usage

## Run tests

```bash
yarn install
yarn build
yarn test
```

## Deployment

Setup a `.env` file following the example of `.env.template`.

```bash
RELEASE_VERSION=v1 npx truffle --network goerli exec scripts/deploy.js
```

## UI

A minimal one-pager plain HTML/jS ui is available also under `ui/` folder.
