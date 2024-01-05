# Changelog
All notable changes to the metadata will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.1.21]
### Changed
- New contract addresses for Resolver and SuperfluidLoader on xdai-mainnet and polygon-mainnet

## [v1.1.20]
### Changed
- New contract addresses for Resolver and SuperfluidLoader on eth-goerli and polygon-mumbai

## [v1.1.19]
### Added
- `cliName` property under "subgraphV1" for the canonical subgraph network names, see [here](https://thegraph.com/docs/en/developing/supported-networks/#hosted-service)

## [v1.1.18]
### Fixed
- Changed the `module/networks/list.d.ts` file to correctly reflect the `contractsV1` object in our `networks.json` file.

## [v1.1.17]
### Fixed
- Removed `governance` from testnets, changes frequently and can't be reliably kept up to date here
- Removed wrong contract entry for xdai-mainnet

## [v1.1.16]
### Fixed
- Fixed `gdaV1` address for `avalanche-fuji`

## [v1.1.15]
### Fixed
- Fixed `existentialNFTCloneFactory` address for `celo-mainnet`

## [v1.1.14]

### Added
- Added `constantOutflowNFT` and `constantInflowNFT`

## [v1.1.13]
### Added
- Added field `existentialNFTCloneFactory` to contract addresses

## [v1.1.12]
### Added
- Added subgraph endpoints for: Autowrap, FlowScheduler and Vesting contracts

## [v1.1.11]

### Added
- Added addresses of autowrap contracts

### Changed
- Node dependency updates.

## [v1.1.10] - 2023-07-25
### Fixes
- Fixed address of SuperTokenFactory for polygon-zkevm-testnet

## [v1.1.9] - 2023-07-19

### Added
- Added `base-mainnet`

## [v1.1.8] - 2023-07-12

### Changed
- Updated Type info of ContractAddresses and NetworkMetaData
- Renamed `zkevm-testnet` => `polygon-zkevm-testnet`
