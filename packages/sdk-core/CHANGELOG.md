# Changelog
All notable changes to the SDK-core will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Map `deposit` to Stream when querying from Subgraph

## [0.6.0] - 2022-11-02

### Added
- Support for mainnet

### Changed
- Framework initialization for supported networks utilizes `@superfluid-finance/metadata`, but still uses the resolver for unsupported/local testing environments
  - > NOTE: This will not create any changes when doing `Framework.create` and is just a minor optimization.
- All transactions executed via SDK-Core have a default multiplier (1.2x) applied to the provider estimated gas limit **unless** an ethers `Overrides` object is passed during creation of the `Operation`.
- There is also the option to pass in an explicit multiplier when executing transactions: `Operation.exec(signer, 1.32)`.

## [0.5.7] - 2022-10-13
### Breaking
-   `CFAv1Forwarder` integrated into SDK-Core and will be the default way of calling CFA agreements and `sender` is now a required property.
  - Migration: pass sender into the affected CFAv1 callAgreement functions - `create/update/deleteFlow`.
  > NOTE: You must pass `shouldUseCallAgreement` explicitly as a property if you want to execute these calls via the Host.

### Added
-   typechain files consumed from `@superfluid-finance/ethereum-contracts` and exported from SDK-Core

## [0.5.6] - 2022-09-07
### Fixes
- Correct `subgraphAPIEndpoint` in `getResolverData`

### Breaking
- Don't wrap `SubgraphClient` with `SFError`

## [0.5.5] - 2022-08-31
### Added
- Support for: `optimism-goerli` and `arbitrum-goerli` added

### Breaking
- Support for: `rinkeby`, `ropsten`, `kovan`, `optimism-kovan` and `arbitrum-rinkeby` removed
- Don't throw `SFError` when executing `Operation` or `BatchCall`; let the original error bubble up

### Fixes
- Serialize a much smaller version of the cause in `SFError` (only `name`, `message`, `code`)
- Change `SFError.name` from "Error" to "SFError"

# [0.5.4] - 2022-08-19

### Fixes
- Properly console the cause, not the caught serialization error

## [0.5.3] - 2022-08-15

### Added
- Map `indexId` to `IndexSubscription` when querying from Subgraph

### Fixes
- Catch and handle serialization error

## [0.5.2] - 2022-07-26

### Added
- Support for `isNativeAssetSuperToken` property on SuperToken entity queries
- `callAppAction` Operation creator added to `Host` class

### Breaking
- `BatchCall.getCallDataFunctionArgs` deprecates the old `BatchCall.getCallAgreementFunctionArgs`
  - Migration:
      - Replace `getCallAgreementFunctionArgs` with `getCallDataFunctionArgs` and pass in the fragment/ABI as the first argument, whilst keeping the same `callData` argument.
- `Host.populateCallAgreementTxnAndReturnOperation` is replaced by `Host.callAgreement`
  - Migration:
      - Replace instances of `populateCallAgreementTxnAndReturnOperation` with `callAgreement`

### Fixes
- Handle `CALL_APP_ACTION` operation type correctly in SDK-Core when doing a batch call
- Undefined `process` in `constants.ts` in React and client-side apps using SDK-Core directly

## [0.5.1] - 2022-07-26

### Fixes
- Patch fix serializeError strange serialization

## [0.5.0] - 2022-07-14

### Added
- Support for SetEvent and Subgraph v1.4.4
- `Framework.operation` method for easily creating `Operation` instances

### Fixes
- Compile AJV validations to prevent unsafe-eval and did not allow SDK-Core usage inside Google Chrome extension due to unsafe-eval CSP

### Changed
- `SFError` refactor to be more conventional. It inherits `Error` and uses `cause` to wrap internal errors.
- Use `serialize-error` for serializing error object inside the message.
- Export Operation & OperationType

### Breaking
- `SFError.errorObject` renamed to `SFError.cause`

## [0.4.4] - 2022-06-30

### Added
- Support for new event properties for Subgraph v1.4.1

### Breaking
- Subgraph Query: `rewardAccount` renamed to `rewardAmountReceiver` on `AgreementLiquidatedV2Event` entity
- `chainId` is a required property for framework initialization
- `networkId` and `dataMode` no longer exist as properties for framework initialization

## [0.4.3] - 2022-06-29

### Added
- BNB Chain support added

### Changed
- `maybeCriticalAtTimestamp` is a nullable property now

## [0.4.2] - 2022-05-17

### Added
- `QueryHandler` for transfer events

## [0.4.2] - 2022-05-17

### Fixed
- Patched SDK-Core Subgraph files to be in sync with V1 Subgraph endpoint

## [0.4.1] - 2022-05-14

### Added
- Avalanche Network Support

### Changed
- Network constants consistent with canonical Superfluid name

## [0.4.0] - 2022-05-06

### Added
- Added option to specify block details when querying through a `SubgraphQueryHandler`
- Added Subgraph's `_meta` table query
- Added `tokenSymbol` for `SubgraphQueryHandler` entity queries where `token` (token ID) was previously included
- Added `PageNumberPaging` for UI development friendly pagination API
- Added `AllPaging` to recursively query all results from Subgraph
- Added support for `TypedDocumentNode` for `SubgraphClient`
- Expose underlying ethers.js contracts for each class: CFAv1, Host, IDAv1 and `contracts` property in `Framework` class
- Added new ACL function support: authorizing flow operator permissions and create/update/delete flow by operator
- Added `nativeTokenSymbol` property to `constants.ts`
- Split `SuperToken` class into: `WrapperSuperToken`, `PureSuperToken` and `NativeAssetSuperToken` classes
- Added `loadWrapperSuperToken`, `loadNativeAssetSuperToken`, and `loadPureSuperToken` super token initialization functions
- Support `upgrade`, `upgradeTo` and `downgrade` functions via `NativeAssetSuperToken`
- Added `upgradeTo` to `WrapperSuperToken` class as this was missing as well

### Changed
- Renamed `Token` to `ERC20Token`
- Exported `ERC20Token`
- Renamed `PagedResult.data` to `PagedResult.items`
- Moved `listAllResults` into separate function from `Query` object
- Removed `SubgraphClient.batchRequests` because Subgraph Node didn't support it

### Internal
- Use `eslint-plugin-prettier` over separate `prettier` instance

### Breaking
- The `SuperToken` class is now an abstract base class and no longer contains the functions `upgrade` and `downgrade`.
- `underlyingToken` is possibly undefined on `SuperToken`: `WrapperSuperToken` has `underlyingToken`, but `PureSuperToken` and `NativeAssetSuperToken` do not.
> NOTE: These changes are due to the split of `SuperToken` into `WrapperSuperToken`, `PureSuperToken` and `NativeAssetSuperToken` classes.
  - Migration:
      - if you are unsure of the type of the super token, you can use: `await framework.loadSuperToken("0x...");`
      - if you want to load a wrapper super token, use: `await framework.loadWrapperSuperToken("DAIx");`
      - if you want to load a native asset super token, use: `await framework.loadNativeAssetSuperToken("ETHx");`
      - if you want to load a pure super token, use: `await framework.loadPureSuperToken("0x...");`

## [0.3.2] - 2022-03-16

### Added

- Added "optimism-mainnet" and "arbitrum-one" support

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

[Unreleased]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.6.0...HEAD
[0.6.0]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.7...sdk-core%40v0.6.0
[0.5.7]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.6...sdk-core%40v0.5.7
[0.5.6]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.5...sdk-core%40v0.5.6
[0.5.5]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.4...sdk-core%40v0.5.5
[0.5.4]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.3...sdk-core%40v0.5.4
[0.5.3]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.2...sdk-core%40v0.5.3
[0.5.2]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.1...sdk-core%40v0.5.2
[0.5.1]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.5.0...sdk-core%40v0.5.1
[0.5.0]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.4.4...sdk-core%40v0.5.0
[0.4.4]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.4.3...sdk-core%40v0.4.4
[0.4.3]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.4.2...sdk-core%40v0.4.3
[0.4.2]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.4.1...sdk-core%40v0.4.2
[0.4.1]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.4.0...sdk-core%40v0.4.1
[0.4.0]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.3.2...sdk-core%40v0.4.0
[0.3.2]: https://github.com/superfluid-finance/protocol-monorepo/compare/sdk-core%40v0.3.1...sdk-core%40v0.3.2
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
