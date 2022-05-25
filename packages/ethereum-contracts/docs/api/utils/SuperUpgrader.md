# Solidity API

## SuperUpgrader

### BACKEND_ROLE

```solidity
bytes32 BACKEND_ROLE
```

### OptoutAutoUpgrade

```solidity
event OptoutAutoUpgrade(address account)
```

### OptinAutoUpgrade

```solidity
event OptinAutoUpgrade(address account)
```

### _optout

```solidity
mapping(address &#x3D;&gt; bool) _optout
```

### constructor

```solidity
constructor(address adminRole, address[] backendAddr) public
```

### upgrade

```solidity
function upgrade(address superTokenAddr, address account, uint256 amount) external
```

The user should ERC20.approve this contract.

_Execute upgrade function in the name of the user_

| Name | Type | Description |
| ---- | ---- | ----------- |
| superTokenAddr | address | Super Token Address to upgrade |
| account | address | User address that previous approved this contract. |
| amount | uint256 | Amount value to be upgraded. |

### isBackendAgent

```solidity
function isBackendAgent(address account) external view returns (bool yes)
```

_Test if account is member BACKEND_ROLE_

### grantBackendAgent

```solidity
function grantBackendAgent(address account) external
```

_Add account to BACKEND_ROLE_

### revokeBackendAgent

```solidity
function revokeBackendAgent(address account) external
```

_Remove account to BACKEND_ROLE_

### getBackendAgents

```solidity
function getBackendAgents() external view returns (address[])
```

_Get list of all members of BACKEND_ROLE_

### optoutAutoUpgrades

```solidity
function optoutAutoUpgrades() external
```

_User signal that opt-out from backend upgrades_

### optinAutoUpgrades

```solidity
function optinAutoUpgrades() external
```

_User signal that revoke opt-out from backend upgrades_

