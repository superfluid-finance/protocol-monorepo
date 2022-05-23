<h1 align="center">Welcome to Superfluid Hot Fuzz 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="https://github.com/superfluid-finance/protocol-monorepo/raw/dev/sf-logo.png" />
</div>

> Testing Superfluid protocol and Super Apps like Simon Pegg.

<center>
    <img src="hot-fuzz-simon.jpg" width="61.803%" />
</center>

Hot-fuzz is a wrapper of [Echidna](https://github.com/crytic/echidna/) with additional helper for fuzzing
your Superfluid smart contracts applications, including [Super Apps](https://docs.superfluid.finance/superfluid/protocol-developers/super-apps).

How To Use
==========

## Setup

1. Download the latest echidna binary from: https://github.com/crytic/echidna

> :warning: Temporary Workaround needed
>
> Hot fuzz currently needs a fix in echidna to support contract deployment to specific address.
>
> The fix can be found at https://github.com/crytic/echidna/pull/758.
>
> To download, open the link above -> click "Checks" tab -> click "CI" -> Download the artifact
> for your perating system.

2. Add `@superfluid-finance/hot-fuzz` to your project `devDependencies`.

`hot-fuzz` does not have a package yet, you should install it through:

```
yarn add --dev 'https://gitpkg.now.sh/api/pkg?url=superfluid-finance/protocol-monorepo/packages/hot-fuzz&commit=dev'
```

> Check out how this works:
>
> - https://github.com/yarnpkg/yarn/issues/4725
> - https://gitpkg.vercel.app/

3. Extend your `truffle-config.js`

It is required to link external libraries correctly during the testing. The workaround is provided, all you need to do
is to add this line to your `truffle-config.js`:

```js
require("@superfluid-finance/hot-fuzz").hotfuzzPatchTruffleConfig(M);
```

> :warning: hardhat-config support is currently missing, pull request please!
>
> Note that there is no harm just to create a minimal truffle-config.js in your project if that helps for now!

:star: Congrats! Now you should be all set!

## Develop A New Hot Fuzzer

1. Create a new hot fuzz contract ending inheriting `HotFuzzBase`.
```solidity
contract YouSuperAppHotFuzz is HotFuzzBase {

    YourApp immutable private _app;

    constructor() HotFuzzBase(10 /* nTesters */ ) {
        // ... setup your app
        _app = new YourApp(sf.host, sf.cfa, superToken);
        ...
        addAccount(address(_app));
    }
```
2. As a convention, the new file name should be `YourApp.hott.sol`.
3. Write additional Echidna yaml configuration, minimally:
```
testMode: "property"
```
Check [Echidna documentation](https://github.com/crytic/echidna/) for more details what you can configure more.
4. Write a list of possible actions how the testers can interact with your app:
```
function participateLottery(uint8 a, int64 flowRate) public {
    LotteryPlayer player = getOnePlayer(a);
    require(flowRate >= _app.MINIMUM_FLOW_RATE());

    player.play(flowRate);
}
```
5. Write additional echidna invariances so that they are checked against through out the hot fuzzing.
Typically you at least don't want your app jailed:
```
function echidna_app_is_free() public view returns (bool) {
    return sf.host.isApp(_app) && !sf.host.isAppJailed(_app);
}
```

> :bulb: Checkout the [flowlottery example](https://github.com/superfluid-finance/protocol-monorepo/tree/dev/examples/flowlottery/solidity-contracts).

## Hot Fuzz It

```
$ npx hot-fuzz contracts/YourAppHotFuzz.yaml
```

Once it is running, what is going on is that your list of actions in addition to a preset of actions defined in
`HotFuzzBase` are randomized as as many sequences of instructions as you configured for, are being executed.

While executing these sequences of instructions, all echidna invariances are checked each time a transaction is made.
Any violation of these invariances are considered a bug somewhere in the app.

That's it, let the tool discover cases for you, have fun hot-fuzzing!

Contribution ✨
===============

The tool is still in early development, many breaking changes may still come.

All contributions are welcome through new issue reports or pull requests.

Let's make smart contracts development safer by testing more.
