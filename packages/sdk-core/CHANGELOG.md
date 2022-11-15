# Changelog
All notable changes to the SDK-core will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.2] - 2022-03-16

### Added

- Added "optimism-mainnet" and "arbitrum-one" support.

## [0.3.1] - 2022-02-16

### Added
- Added `indexValueCurrent` to `IndexSubscription` query to optimize calculating "total amount distributed" in consuming applications ([#629])
- Added `indexTotalUnits` to `IndexSubscription` query to optimize calculating "pool percentage" in consuming applications ([#630])

### Fixed
- Typo for `networkName: "arbitrum-rinkeby"` fixed (was expecting `"arbitrium-rinkeby"`) in `Framework.create` ([#637])

### Breaking
- Using `"xdai"` as the `networkName` will no longer work. Updated to `"gnosis"` 
  - Migration: change `networkName` from `"xdai"` to `"gnosis"`

## [0.3.0] - 2022-02-02
### Added
- New query handlers to cover full spectrum of Subgraph querying capabilities
  - Used by the new release of SDK-redux
  - Lacks an easy-to-use API for average SDK-core user

### Changed
- `_ethers` is not exported for UMD build anymore. Use `window._ethers` instead.
- `SFError` and `ErrorType` are now exported
- Generated Subgraph types are now exported (`*_Filter` and `*_OrderBy` types)
    - There are a lot of Subgraph types, so it does make the namespace crowded. We'll alleviate it in a later release.

## [0.2.1] - 2022-01-31

### Added
- Load `SuperToken` by token symbol as long as the token is listed on the resolver ([#588])

## [0.2.0] - 2022-01-14
### Added
- Support for new testnets: Optimism Kovan, Avalanche Fuji, and Arbitrum Rinkeby ([#526])
- Working UMD build added to package and removed duplicated .d.ts typing files from package ([#535])
- ethers.js overrides object support added to all `Operation` creation functions ([#540])
- `Query` class overhaul: fuller filter and ordering and more generation of types

### Changed
- package.json cleanup and subgraph schema updated ([#522])
- Simpler provider logic for Framework creation ([#537])
- Use V1 instead of devSubgraph endpoints ([#550])
- `superTokenFactoryAddress` removed from `IConfig` interface ([#556])

### Fixed
- Outdated README.md cleanup and updates ([#520], [#524], [#526], [#530], [#537], [#549], [#556])
- `BatchCall` for callAgreement functions fixed, flow rate calculation utils logic fixed ([#526])
- Improper base 18 number for flow rate calculation utils ([#545])
- single network deploy implemented ([#506])
- eslint errors and .json imports fixed ([#535])

### Breaking
- Breaking change: Framework.create interface breaking change:`web3Ethers` and `hardhatEthers` properties removed ([#537])
  - Migration: if you were using one of these two properties, change the property `web3Ethers` and `hardhatEthers` to `provider`
- Breaking change: non Framework initialized `batchCall` constructor arguments interface change: `config` property removed ([#556])
  - Migration: if you instantiated a `BatchCall` without using Framework, you need to remove the config property and replace it with the property: `hostAddress`

## [0.1.0] - 2021-12-01
### Added
- Initial preview version of SDK-Core
- Features:
  - New `Framework` initialization pattern
  - Built with `ethers.js` and `TypeScript` from the ground up
  - `Query` class which leverages the Subgraph for queries with simple filters
  - New `Operation` class for executing transactions/batching transactions
  - `ConstantFlowAgreementV1` and `InstantDistributionAgreementV1` helper classes with create, read, update and delete functionality
  - New `SuperToken` class with `SuperToken` CRUD functionality and an underlying `Token` class with basic `ERC20` functionality
  - New `BatchCall` class for creating and executing batch calls with supported `Operation's`

[Unreleased]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.3.1...HEAD
[0.3.1]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.3.0...sdk-core%40v0.3.1
[0.3.0]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.2.1...sdk-core%40v0.3.0
[0.2.1]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.2.0...sdk-core%40v0.2.1
[0.2.0]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.1.0...sdk-core%40v0.2.0
[0.1.0]: https://github.com/superfluid-finance/protocol-monorepo/releases/tag/sdk-core%40v0.1.0


[#506]: https://github.com/superfluid-finance/protocol-monorepo/pull/506
[#515]: https://github.com/superfluid-finance/protocol-monorepo/pull/515
[#520]: https://github.com/superfluid-finance/protocol-monorepo/pull/520
[#522]: https://github.com/superfluid-finance/protocol-monorepo/pull/522
[#524]: https://github.com/superfluid-finance/protocol-monorepo/pull/524
[#526]: https://github.com/superfluid-finance/protocol-monorepo/pull/526
[#530]: https://github.com/superfluid-finance/protocol-monorepo/pull/530
[#535]: https://github.com/superfluid-finance/protocol-monorepo/pull/535
[#537]: https://github.com/superfluid-finance/protocol-monorepo/pull/537
[#540]: https://github.com/superfluid-finance/protocol-monorepo/pull/540
[#545]: https://github.com/superfluid-finance/protocol-monorepo/pull/545
[#549]: https://github.com/superfluid-finance/protocol-monorepo/pull/549
[#550]: https://github.com/superfluid-finance/protocol-monorepo/pull/550
[#556]: https://github.com/superfluid-finance/protocol-monorepo/pull/556
[#588]: https://github.com/superfluid-finance/protocol-monorepo/pull/588
[#629]: https://github.com/superfluid-finance/protocol-monorepo/pull/629
[#630]: https://github.com/superfluid-finance/protocol-monorepo/pull/630
[#630]: https://github.com/superfluid-finance/protocol-monorepo/pull/630
[#637]: https://github.com/superfluid-finance/protocol-monorepo/pull/637
