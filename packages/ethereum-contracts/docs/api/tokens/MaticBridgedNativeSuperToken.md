# MaticBridgedNativeSuperTokenProxy

Native SuperToken with interfaces for the Matic POS bridge to mint and burn.
See https://docs.polygon.technology/docs/develop/ethereum-matic/pos/mapping-assets/

## Functions

### constructor

```solidity
function constructor(
    address childChainManager_
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `childChainManager_` | address |  |

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

