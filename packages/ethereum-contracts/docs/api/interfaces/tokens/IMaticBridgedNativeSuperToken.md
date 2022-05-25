# IMaticBridgedNativeSuperTokenCustom

Functionality specific for Matic Bridged Native Super Tokens

## Functions

### deposit

```solidity
function deposit(
    address user,
    bytes depositData
) external
```

triggers minting of tokens to the given user, called by the child chain manager

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `user` | address |  |
| `depositData` | bytes |  |

### withdraw

```solidity
function withdraw(
    uint256 amount
) external
```

triggers burning of tokens on the child chain and unlocking on L1

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 |  |

### updateChildChainManager

```solidity
function updateChildChainManager(
    address newChildChainManager
) external
```

governance can change the child chain manager

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newChildChainManager` | address |  |

## Events

### ChildChainManagerChanged

```solidity
event ChildChainManagerChanged(
    address newAddress
)
```

emitted when the child chain manager changes

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newAddress` | address |  |

# IMaticBridgedNativeSuperToken

Matic Bridged Native SuperToken full interface

