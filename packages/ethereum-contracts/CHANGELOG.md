# Changelog
All notable changes to the ethereum-contracts will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Breaking

- `TokenInfo` and `ERC20WithTokenInfo` interface/abstract contract are removed from the codebase, including the bundled ABI contracts
  - Migration: Use `IERC20Metadata` instead, as this replaces the previous contracts
- `build/typechain-ethers-v5` is removed from the npm package
  - Migration: Consume the ABIs and utilize whatever tool you'd like to generate types from them

### Added

- Added 'test-slither' yarn sub-task.
- Expose `SuperToken._underlyingDecimals` with `SuperToken.getUnderlyingDecimals()`
- Expose `_toUnderlyingAmount(uint256 amount)` with `toUnderlyingAmount(uint256 amount)`
- `batchCall` supports payable `OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION`: only the first `OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION` will be payable
- Added two new functions to `SuperfluidGovernanceBase.sol`: `changeSuperTokenAdmin` and `batchChangeSuperTokenAdmin`
- `Superfluid.changeSuperTokenAdmin()` function added to be called via governance for tokens with no admin address
- Added an overloaded `initialize` to `SuperToken.sol`, which additionally takes `address admin` if you want to initialize the token with an admin address
- `SuperToken.changeAdmin(address newAdmin)` added which is only callable by the current admin, the "admin" of a SuperToken can change the admin and update the proxy contract's pointer to a logic contract
  > Note that the default admin (when address(0)) is the host contract as is currently the case
- Note that the admin is stored in the EIP-1967 admin storage slot (`0xb53127684a568b3173ae13b9f8a6016e243e63b6e8ee1178d6a717850b5d6103`)
- `SuperToken.getAdmin()` added to retrieve the admin address
- `SuperTokenFactory.createERC20Wrapper()` overloads added to create a SuperToken AND explicitly initialize a SuperToken with an admin
- New explicit functions: `deployTestFrameworkWithEthersV5` and `deployTestFrameworkWithEthersV6` in `deploy-test-framework.js`
  - `deployTestFramework` is still there, but it is considered deprecated now

### Changed

- Reuse config keys from `SuperfluidGovernanceConfigs` instead of duplicating them in `ConstantFlowAgreementV1`.
- Deprecating `registerAppWithKey` and `registerAppByFactory`: DO NOT USE for new deployments
  - Simplification of Super App registration: use `registerApp` in all cases going forward.
  - Use `registerApp(uint256 configWord)` to be called by the super app in the constructor or `registerApp(ISuperApp app, uint256 configWord)` to be called by any address with a valid app registration config key

### Fixes
- [`dev-scripts/deploy-test-framework.js`](dev-scripts/deploy-test-framework.js) compatible with both ethers-v5 and ethers-v6 now

## [v1.8.1] - 2023-08-28

### Fixed

- Publish `build/hardhat/*/*` again.

## [v1.8.0] - 2023-08-23

### Added

- Publish `build/bundled-abi.{js,json}` in the npm package.

### Breaking

- Publish `build/truffle/*.json` truffle contract artifacts in the npm package instead.
- Publish `build/typechain-ethers-v5` in the npm package instead.
- Removed scripts from npm package, use dev-scripts instead.

### Fixes

- Ship ERC1820Registry.json with dev-scripts.

### Changed

- Node dependency updates.
- Raise minimum solc development requirement for ethereum-contracts to 0.8.11.

## [v1.7.2] - 2023-08-11

### Added
- `increaseFlowRateAllowanceWithPermissions` and `decreaseFlowRateAllowanceWithPermissions` added to `ConstantFlowAgreementV1.sol`

### Changed
- `SuperToken.sol` made external and public methods virtual to facilitate creation of customized implementations.
- Explicitly set EVM target to "paris" because EIP-3855 isn't yet supported on all chains with Superfluid deployment.
- `SuperAppBaseFlow.sol` uses `registerAppWithKey` instead of deprecated `registerApp`
- `SuperfluidGovernanceBase.sol` add a method for updating SuperTokens to other than the canonical implementation.
- `SuperToken.sol` invoke mint/burn hooks only if userData is not empty

## [v1.7.1] - 2023-06-27

### Added
- `SuperfluidLoaderLibrary.sol` added to easily get the canonical host contract address on different networks
- `IDAv1Forwarder.sol` added

### Changed
- Dual licenses going forward: AGPLv3 for core contracts and MIT for external developers.

## [v1.7.0] - 2023-06-09
### Breaking
- `SuperfluidFrameworkDeployer.sol` refactored, deployment no longer occurs in the constructor
  - Migration: Create the contract and then use `deployTestFramework` function to execute the deployments
- `SuperTokenDeployer.sol` removed
  - Migration: Remove usage of `SuperTokenDeployer` and use the `SuperfluidFrameworkDeployer` to deploy tokens instead
- `FlowNFTBase.sol` no longer takes `baseURI` in the constructor, it is now a constant value
  - Migration: Remove `baseURI` from the constructor for `ConstantOutflowNFT` and `ConstantInflowNFT`

### Changed
- `SuperTokenDeployer.sol` removed in favor of moving the token deployment functions to `SuperfluidFrameworkDeployer.sol`
- `SuperfluidFrameworkDeployer.sol` also includes functions which allow you to deploy different parts of the protocol (core, agreements, super tokens, etc.) separately and provides helpful custom errors for debugging
- Use custom error for out of gas condition in `Superfluid.sol`

### Added
- `increaseFlowRateAllowance` and `decreaseFlowRateAllowance` added to `SuperTokenV1Library.sol`
- Testing speed benchmarks

### Fixed
- `SuperTokenFactory.sol` NFT upgrade logic fixed
- `SuperTokenV1Library.sol` IDA keccak256 hash fixed
- `deploy-framework.js` script sets CFAv1Forwarder as trusted forwarder
- `deploy-framework.js` script fixed up for flow NFT contracts

## [v1.6.0] - 2023-04-26
### Added
- FlowNFT contracts: `ConstantOutflowNFT`, `ConstantInflowNFT`, `FlowNFTBase` to replace the OG flow NFTs
- `SuperAppBaseCFA` base contract to simplify CFA SuperApp development
- `SuperTokenDeployer.sol` for deploying SuperTokens in local testing (split from `SuperfluidFrameworkDeployer.sol`)

### Changed
- `SuperToken` logic contract takes `ConstantOutflowNFT` and `ConstantInflowNFT` proxy contract addresses
- `SuperTokenFactory` logic contract takes `ConstantOutflowNFT` and `ConstantInflowNFT` and handles the upgrade logic for the NFTs
- Utilize internal `_host` in CFA instead of external call to host on the token

## [v1.5.2] - 2023-03-14

### Added
- bump solc to 0.8.19
- New ACL functions: `increaseFlowRateAllowance` and `decreaseFlowRateAllowance` in `ConstantFlowAgreementV1.sol`
- Support for `superToken.increaseAllowance` and `superToken.decreaseAllowance` in `batchCall` in `Superfluid.sol`

### Breaking
- `BatchLiquidator.deleteFlows` doesn't take host and CFA address as argument anymore. This makes L2 solvency operations considerably cheaper.

### Changed
- Added `BatchLiquidator.deleteFlow` for cheaper liquidation of singular flows on L2s.

## [v1.5.1] - 2023-02-28

### Added
- bump solc to 0.8.18

### Breaking
- `SuperTokenFactory` contract no longer takes `SuperTokenHelper` contract in its constructor
  - Migration: Pass in a deployed `SuperToken` (logic) contract address to `SuperTokenFactory` constructor instead
- `runDeployContractsAndToken.js` is renamed to `run-deploy-contracts-and-token.js`
  - Migration: Replace `runDeployContractsAndToken.js` with `run-deploy-contracts-and-token.js` instead in imports
- `deployContractsAndToken.js` is renamed to `deploy-contracts-and-token.js`
  - Migration: Replace `deployContractsAndToken.js` with `deploy-contracts-and-token.js` instead in imports

### Changed
- `_superTokenLogic` field in `SuperTokenFactory` contract is now a public immutable field and is no longer a storage variable: `_superTokenLogicDeprecated`

## [v1.5.0] - 2022-12-19
### Added
- `batchCall` supports new `send` batch operation
- Added `downgradeTo` function in `SuperToken.sol`
- Added `deployContractsAndToken.js` and `runDeployContractsAndToken.js` to `dev-scripts`
- Added `SuperTokenV1Library` which provides a token centric interface for Superfluid specific SuperToken functionality.

### Breaking

- `scripts/deploy-test-framework.js` no longer exists, `deploy-test-framework.js` has been moved to `dev-scripts`
  - Migration: Use `dev-scripts/deploy-test-framework.js` instead

### Changed

- `upgradeTo` logic changed to revert if `data` is not empty and `to` is a contract and is not a registered ERC777 recipient
- `MAX_APP_CALLBACK_LEVEL` is public again

## [v1.4.3] - 2022-10-27
### Added

- `createCanonicalERC20Wrapper` added for creating ERC20 Wrapper Super tokens which will be added to a canonical wrapper super token list based on naming convention and semi-upgradeability. This will be the recommended way of creating ERC20 Wrapper moving forward.
  - `name` naming convention: Super Token `name` will be `"Super ${underlyingToken.name}"`
  - `symbol` naming convention: Super Token `symbol` will be `"${underlyingToken.symbol}x"`
- Hardhat `artifacts` included in npm package
- Include declaration files in `types` folder for files in `scripts` in npm package

## [v.1.4.2] - 2022-10-13
### Added

- CFA Hooks added (#1099)
- typechain types are provided to all consumers (#1113)
- `SuperfluidFrameworkDeployer.sol` create Native Asset and Pure Super Tokens functionality added (#1104)
- `TestResolver` added to allow `SuperfluidFrameworkDeployer` contract to grant admin permissions to its deployer (#1104)

### Fixes

- CFA Agreement forwarder delete flow fix to allow receivers to delete flow (#1094)

## [v.1.4.1] - 2022-09-20

### Added

- CFA Agreement forwarder: deployed on all networks at 0xCfA1E187C9141B8bA90a436CB789017FA573d051

### Fixes

- [SECURITY] CFA: check flow sender instead of msg sender in order to cover ACL use

## [v.1.4.0] - 2022-09-05

### Added

- CFA Agreement Forwarder (#982)
- Gov: Convenience method for app registration keys and some cleanup (#1049)
- App Credit Rule CFA-2 (#899)
- Custom Errors Support (#1043)
- yAcademy Gas Savings Applied (#1062)
- bump solc to 0.8.16 (#1065)
- Add gnosisscan support (#1050)
- BNB Chain Support (#925)
- Initialize logic contracts on deployment (using "castrate") (#841)
- add tokenDecimals option to deploy-test-token.js (#1025)

### Fixes

- [TOGA] Added missing awaits (#1044)
- New TestSuperApp: stream redirector wip, for testing when `appCallbackLevel` starting at 0 (#1008)
- Prod deployment script fix (#971)
- Adds a missing closing parenthesis to example test (#967)
- Addresses several shortcomings in current workflows (mainly mainnet related) (#911)
- Queue up FAILED_VERIFICATIONS (#900)

### Changed

- Deprecate support for `kovan`, `rinkeby`, `optimism-kovan`, and `optimism-rinkeby` (#1032)
- Remove MaticBridgedToken from monorepo (#1060)
- Updating dependencies & dedup dev packages (#1064)

## [v1.3.1] - 2022-08-05

### Added

- Add resolver and loader to deployer, add tests (#1011)

## [v1.3.0] - 2022-06-16

### Added

- Automated generation and publishing of contract API docs with solidity-docgen (#880)
- Set newCtx whenever a function is modifying + using it (#854)
- CfAv1Library ACL support (#803)
- Avalanche Support Added (#819)

### Fixes

- Production deployment script fixes (#821)
- Increase Code Coverage (#891)
- SlotsBitmapLibrary Property Test Fuzzing (#790)

### Changed

- IDA: Distribute no longer reverts when units is 0 (#853)
- Use `abi.encodeCall` (#869)
- Review safemath & safecast (#851)
- Solidity 0.8.14 (#896)

## [v1.2.2] - 2022-05-09

### Breaking

- SuperfluidFrameworkDeployer.getFramework to return a Framework struct. (#789)

### Fixes

- Fixed a natspec typo (#773)
- Reapply OpenEthereum provider hack (#771)

### Added

- ERC1820RegistryCompiled for testing frameworks: foundry, etc. (#789)
- Add few more CFA and IDA cases (#781)
- Test some CFA properties (#777)

### Changed

- App registration keys with expiration date instead of one-time use.

## [v1.2.1] - 2022-04-28

### Breaking

- ACL authorize functions no longer take a redundant `sender` param

## [v1.2.0] - 2022-04-28

### Added

- Added HALBORN report (#759)
- **MAJOR FEATURE**: Access Control List (#720)
  - ACL base API functions
    - FlowOperatorDefinitions library
    - new functions in IConstantFlowAgreementV1
  - Other changes:
    - emit FlowUpdatedExtension event

### Fixes

- Fix ERC1820 artifact issue (#759)
- Fix an issue with output dir (#748)
- [SECURITY] InstantDistributionAgreement.claim should be guarded by ctx check.
- Interface renaming fix re patrician periods (#747)

### Changed

- Remove Strings.sol again
- Disable zero address subscription for IDA (#751)
- Update dev dependencies (#694)
- Use hardhat for ethereum-contracts tests (#727)
- Update to solc 0.8.13 (#721)

### Improvements

- Reduce trust assumptions host <-> agreements (#749)
- Additional IDA test cases (#722)

## [v1.1.1] - 2022-03-18

### Fixes

- Add Strings.sol back to repository (#715)

## [v1.1.0] - 2022-03-18

### Changed

- Use host.getNow to fully abstract time (#703)
- Use Solidity v0.8.0 (#687)
- Informative revertFromReturnedData (#693)

### Added

- IDAv1Library (#696)
- Added single-file Dapp for deploying SuperToken wrappers (#692)
- Print contract sizes (#690)
- Workflow Improvements (cloc and faster pre-commit) (#688)

### Fixes

- Info-show-protocol fix for PPP and cleanup (#684)

### Breaking

- Monogamous SETH, L2 config improvements (#707).
- All interface contracts are requiring you to use solc >= 0.8.0.
- Removed ERC777 callbacks from ERC20.transfer (#698).
- [SECURITY] isValidAppAction added, this prevents SuperApp callbacks being mistakenly called by callAppActions.

## [v1.0.0] - 2022-03-03

### Changed

- Upgrade `truffle-plugin-verify` and use its proxy verification feature (#623)

### Added

- 3Ps (#558)
- Add SuperApp factory registration script (#657)
- CFALibraryV1: added flow() and flowWithCtx() functions (#595)
- Self approve transfer (#598)
- External set/clear config added to governance (#624)

### Fixes

- Add missing event emits (#656)
- CFALibraryV1: Change solidity pragma in cfa lib to support versions 0.8 & later (#642)
- Multiple scripts fixes re: do not engage with uninitialized super token proxies (#603)

### Security

- Call agreement calldata exploit (#628)
- Internal code audit iteration one (#646)

### Technical Debt Cleanups

- Techdebt cleanups (#673)
- UUPS review (#632)
