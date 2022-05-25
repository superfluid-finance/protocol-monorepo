# Solidity API

## SuperfluidToken

### _REWARD_ADDRESS_CONFIG_KEY

```solidity
bytes32 _REWARD_ADDRESS_CONFIG_KEY
```

### _host

```solidity
contract ISuperfluid _host
```

_Superfluid contract_

### _inactiveAgreementBitmap

```solidity
mapping(address &#x3D;&gt; uint256) _inactiveAgreementBitmap
```

_Active agreement bitmap_

### _balances

```solidity
mapping(address &#x3D;&gt; int256) _balances
```

_Settled balance for the account_

### _totalSupply

```solidity
uint256 _totalSupply
```

_Total supply_

### _reserve4

```solidity
uint256 _reserve4
```

### _reserve5

```solidity
uint256 _reserve5
```

### _reserve6

```solidity
uint256 _reserve6
```

### _reserve7

```solidity
uint256 _reserve7
```

### _reserve8

```solidity
uint256 _reserve8
```

### _reserve9

```solidity
uint256 _reserve9
```

### _reserve10

```solidity
uint256 _reserve10
```

### _reserve11

```solidity
uint256 _reserve11
```

### _reserve12

```solidity
uint256 _reserve12
```

### _reserve13

```solidity
uint256 _reserve13
```

### constructor

```solidity
constructor(contract ISuperfluid host) internal
```

### getHost

```solidity
function getHost() external view returns (address host)
```

_ISuperfluidToken.getHost implementation_

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(address account, uint256 timestamp) public view returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit)
```

_ISuperfluidToken.realtimeBalanceOf implementation_

### realtimeBalanceOfNow

```solidity
function realtimeBalanceOfNow(address account) public view returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit, uint256 timestamp)
```

_ISuperfluidToken.realtimeBalanceOfNow implementation_

### isAccountCritical

```solidity
function isAccountCritical(address account, uint256 timestamp) public view returns (bool isCritical)
```

Check if account is critical

_A critical account is when availableBalance &lt; 0_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to check |
| timestamp | uint256 | The time we&#x27;d like to check if the account is critical (should use future) |

| Name | Type | Description |
| ---- | ---- | ----------- |
| isCritical | bool | Whether the account is critical |

### isAccountCriticalNow

```solidity
function isAccountCriticalNow(address account) external view returns (bool isCritical)
```

Check if account is critical now (current host.getNow())

_A critical account is when availableBalance &lt; 0_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to check |

| Name | Type | Description |
| ---- | ---- | ----------- |
| isCritical | bool | Whether the account is critical |

### isAccountSolvent

```solidity
function isAccountSolvent(address account, uint256 timestamp) public view returns (bool isSolvent)
```

Check if account is solvent

_An account is insolvent when the sum of deposits for a token can&#x27;t cover the negative availableBalance_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to check |
| timestamp | uint256 | The time we&#x27;d like to check if the account is solvent (should use future) |

| Name | Type | Description |
| ---- | ---- | ----------- |
| isSolvent | bool | True if the account is solvent, false otherwise |

### isAccountSolventNow

```solidity
function isAccountSolventNow(address account) external view returns (bool isSolvent)
```

Check if account is solvent now

_An account is insolvent when the sum of deposits for a token can&#x27;t cover the negative availableBalance_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to check |

| Name | Type | Description |
| ---- | ---- | ----------- |
| isSolvent | bool | True if the account is solvent, false otherwise |

### getAccountActiveAgreements

```solidity
function getAccountActiveAgreements(address account) public view returns (contract ISuperAgreement[])
```

_ISuperfluidToken.getAccountActiveAgreements implementation_

### _mint

```solidity
function _mint(address account, uint256 amount) internal
```

### _burn

```solidity
function _burn(address account, uint256 amount) internal
```

### _move

```solidity
function _move(address from, address to, int256 amount) internal
```

### createAgreement

```solidity
function createAgreement(bytes32 id, bytes32[] data) external
```

_ISuperfluidToken.createAgreement implementation_

### getAgreementData

```solidity
function getAgreementData(address agreementClass, bytes32 id, uint256 dataLength) external view returns (bytes32[] data)
```

_ISuperfluidToken.getAgreementData implementation_

### updateAgreementData

```solidity
function updateAgreementData(bytes32 id, bytes32[] data) external
```

_ISuperfluidToken.updateAgreementData implementation_

### terminateAgreement

```solidity
function terminateAgreement(bytes32 id, uint256 dataLength) external
```

_ISuperfluidToken.terminateAgreement implementation_

### updateAgreementStateSlot

```solidity
function updateAgreementStateSlot(address account, uint256 slotId, bytes32[] slotData) external
```

_ISuperfluidToken.updateAgreementState implementation_

### getAgreementStateSlot

```solidity
function getAgreementStateSlot(address agreementClass, address account, uint256 slotId, uint256 dataLength) external view returns (bytes32[] slotData)
```

_ISuperfluidToken.getAgreementState implementation_

### settleBalance

```solidity
function settleBalance(address account, int256 delta) external
```

_ISuperfluidToken.settleBalance implementation_

### makeLiquidationPayoutsV2

```solidity
function makeLiquidationPayoutsV2(bytes32 id, bytes liquidationTypeData, address liquidatorAccount, bool useDefaultRewardAccount, address targetAccount, uint256 rewardAmount, int256 targetAccountBalanceDelta) external
```

_ISuperfluidToken.makeLiquidationPayoutsV2 implementation_

### onlyAgreement

```solidity
modifier onlyAgreement()
```

### onlyHost

```solidity
modifier onlyHost()
```

