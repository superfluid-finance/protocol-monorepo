# SuperfluidToken

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

### getHost

```solidity
function getHost(
) external returns (address host)
```

ISuperfluidToken.getHost implementation

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(
    address account,
    uint256 timestamp
) public returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit)
```

ISuperfluidToken.realtimeBalanceOf implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `timestamp` | uint256 |  |

### realtimeBalanceOfNow

```solidity
function realtimeBalanceOfNow(
    address account
) public returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit, uint256 timestamp)
```

ISuperfluidToken.realtimeBalanceOfNow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### isAccountCritical

```solidity
function isAccountCritical(
    address account,
    uint256 timestamp
) public returns (bool isCritical)
```

Check if account is critical

A critical account is when availableBalance < 0

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to check |
| `timestamp` | uint256 | The time we'd like to check if the account is critical (should use future) |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `isCritical` | bool | Whether the account is critical |

### isAccountCriticalNow

```solidity
function isAccountCriticalNow(
    address account
) external returns (bool isCritical)
```

Check if account is critical now (current host.getNow())

A critical account is when availableBalance < 0

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to check |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `isCritical` | bool | Whether the account is critical |

### isAccountSolvent

```solidity
function isAccountSolvent(
    address account,
    uint256 timestamp
) public returns (bool isSolvent)
```

Check if account is solvent

An account is insolvent when the sum of deposits for a token can't cover the negative availableBalance

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to check |
| `timestamp` | uint256 | The time we'd like to check if the account is solvent (should use future) |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `isSolvent` | bool | True if the account is solvent, false otherwise |

### isAccountSolventNow

```solidity
function isAccountSolventNow(
    address account
) external returns (bool isSolvent)
```

Check if account is solvent now

An account is insolvent when the sum of deposits for a token can't cover the negative availableBalance

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to check |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `isSolvent` | bool | True if the account is solvent, false otherwise |

### getAccountActiveAgreements

```solidity
function getAccountActiveAgreements(
    address account
) public returns (contract ISuperAgreement[])
```

ISuperfluidToken.getAccountActiveAgreements implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### _mint

```solidity
function _mint(
    address account,
    uint256 amount
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |

### _burn

```solidity
function _burn(
    address account,
    uint256 amount
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |

### _move

```solidity
function _move(
    address from,
    address to,
    int256 amount
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `from` | address |  |
| `to` | address |  |
| `amount` | int256 |  |

### createAgreement

```solidity
function createAgreement(
    bytes32 id,
    bytes32[] data
) external
```

ISuperfluidToken.createAgreement implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `id` | bytes32 |  |
| `data` | bytes32[] |  |

### getAgreementData

```solidity
function getAgreementData(
    address agreementClass,
    bytes32 id,
    uint256 dataLength
) external returns (bytes32[] data)
```

ISuperfluidToken.getAgreementData implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClass` | address |  |
| `id` | bytes32 |  |
| `dataLength` | uint256 |  |

### updateAgreementData

```solidity
function updateAgreementData(
    bytes32 id,
    bytes32[] data
) external
```

ISuperfluidToken.updateAgreementData implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `id` | bytes32 |  |
| `data` | bytes32[] |  |

### terminateAgreement

```solidity
function terminateAgreement(
    bytes32 id,
    uint256 dataLength
) external
```

ISuperfluidToken.terminateAgreement implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `id` | bytes32 |  |
| `dataLength` | uint256 |  |

### updateAgreementStateSlot

```solidity
function updateAgreementStateSlot(
    address account,
    uint256 slotId,
    bytes32[] slotData
) external
```

ISuperfluidToken.updateAgreementState implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `slotId` | uint256 |  |
| `slotData` | bytes32[] |  |

### getAgreementStateSlot

```solidity
function getAgreementStateSlot(
    address agreementClass,
    address account,
    uint256 slotId,
    uint256 dataLength
) external returns (bytes32[] slotData)
```

ISuperfluidToken.getAgreementState implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClass` | address |  |
| `account` | address |  |
| `slotId` | uint256 |  |
| `dataLength` | uint256 |  |

### settleBalance

```solidity
function settleBalance(
    address account,
    int256 delta
) external
```

ISuperfluidToken.settleBalance implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `delta` | int256 |  |

### makeLiquidationPayoutsV2

```solidity
function makeLiquidationPayoutsV2(
    bytes32 id,
    bytes liquidationTypeData,
    address liquidatorAccount,
    bool useDefaultRewardAccount,
    address targetAccount,
    uint256 rewardAmount,
    int256 targetAccountBalanceDelta
) external
```

ISuperfluidToken.makeLiquidationPayoutsV2 implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `id` | bytes32 |  |
| `liquidationTypeData` | bytes |  |
| `liquidatorAccount` | address |  |
| `useDefaultRewardAccount` | bool |  |
| `targetAccount` | address |  |
| `rewardAmount` | uint256 |  |
| `targetAccountBalanceDelta` | int256 |  |

