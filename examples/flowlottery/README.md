Flow Lottery Game
=================

A game of chance built on Superfluid. Users join the game by sending a stream to our contract.
All incoming streams are summed and the resulting reward stream is sent to the winner, until a new one is chosen!

Read more about how to build this game from [this medium article](https://medium.com/superfluid-blog/hacking-on-superfluid-bbb9ade94f98).

A version works for the goerli testnet is also deployed to [https://flowlottery.eth.link/](https://flowlottery.eth.link/).

Development
===========

## Build contracts

```
$ npm ci
$ npm run test
$ npm run build
```

## Deploy the SuperApp

```
$ npm run deploy
```

## Test the UI

To use the already deployed the SuperApp:

```
$ cd ui
$ npm ci
$ npm start
```

## Hack It

- `contracts/LotterySuperApp.sol`: The flow lottery SuperApp.
- `test/LotterySuperApp.test.js`: The test cases.
- `ui`: The flow lottery dapp UI.
