# Solidity API

## TestGovernance

_A initializable version of the governance for testing purpose_

### _host

```solidity
contract ISuperfluid _host
```

### initialize

```solidity
function initialize(contract ISuperfluid host, address rewardAddress, uint256 liquidationPeriod, uint256 patricianPeriod, address[] trustedForwarders) external
```

### _requireAuthorised

```solidity
function _requireAuthorised(contract ISuperfluid host) internal view
```

