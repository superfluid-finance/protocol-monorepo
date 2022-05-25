# Solidity API

## MaticBridgedNativeSuperTokenProxy

_Native SuperToken with interfaces for the Matic POS bridge to mint and burn.
See https://docs.polygon.technology/docs/develop/ethereum-matic/pos/mapping-assets/_

### childChainManager

```solidity
address childChainManager
```

### constructor

```solidity
constructor(address childChainManager_) public
```

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

