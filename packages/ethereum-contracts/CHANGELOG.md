# Changelog
All notable changes to the ethereum-contracts will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### [v1.4.3] - 2022-10-27
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
