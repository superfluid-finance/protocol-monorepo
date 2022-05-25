# SuperUpgrader

## Functions

### constructor

```solidity
function constructor(
    address adminRole,
    address[] backendAddr
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `adminRole` | address |  |
| `backendAddr` | address[] |  |

### upgrade

```solidity
function upgrade(
    address superTokenAddr,
    address account,
    uint256 amount
) external
```

The user should ERC20.approve this contract.

Execute upgrade function in the name of the user

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superTokenAddr` | address | Super Token Address to upgrade |
| `account` | address | User address that previous approved this contract. |
| `amount` | uint256 | Amount value to be upgraded. |

### isBackendAgent

```solidity
function isBackendAgent(
    address account
) external returns (bool yes)
```

Test if account is member BACKEND_ROLE

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### grantBackendAgent

```solidity
function grantBackendAgent(
    address account
) external
```

Add account to BACKEND_ROLE

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### revokeBackendAgent

```solidity
function revokeBackendAgent(
    address account
) external
```

Remove account to BACKEND_ROLE

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### getBackendAgents

```solidity
function getBackendAgents(
) external returns (address[])
```

Get list of all members of BACKEND_ROLE

### optoutAutoUpgrades

```solidity
function optoutAutoUpgrades(
) external
```

User signal that opt-out from backend upgrades

### optinAutoUpgrades

```solidity
function optinAutoUpgrades(
) external
```

User signal that revoke opt-out from backend upgrades

## Events

### OptoutAutoUpgrade

```solidity
event OptoutAutoUpgrade(
    address account
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
### OptinAutoUpgrade

```solidity
event OptinAutoUpgrade(
    address account
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

