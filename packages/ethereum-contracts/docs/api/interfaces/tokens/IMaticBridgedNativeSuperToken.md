# Solidity API

## IMaticBridgedNativeSuperTokenCustom

_Functionality specific for Matic Bridged Native Super Tokens_

### deposit

```solidity
function deposit(address user, bytes depositData) external
```

_triggers minting of tokens to the given user, called by the child chain manager_

### withdraw

```solidity
function withdraw(uint256 amount) external
```

_triggers burning of tokens on the child chain and unlocking on L1_

### updateChildChainManager

```solidity
function updateChildChainManager(address newChildChainManager) external
```

_governance can change the child chain manager_

### ChildChainManagerChanged

```solidity
event ChildChainManagerChanged(address newAddress)
```

_emitted when the child chain manager changes_

## IMaticBridgedNativeSuperToken

_Matic Bridged Native SuperToken full interface_

