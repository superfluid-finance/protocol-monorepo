# Solidity API

## ISuperAgreement

### agreementType

```solidity
function agreementType() external view returns (bytes32)
```

_Get the type of the agreement class_

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(contract ISuperfluidToken token, address account, uint256 time) external view returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

_Calculate the real-time balance for the account of this agreement class_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken |  |
| account | address | Account the state belongs to |
| time | uint256 | Time used for the calculation |

| Name | Type | Description |
| ---- | ---- | ----------- |
| dynamicBalance | int256 | Dynamic balance portion of real-time balance of this agreement |
| deposit | uint256 | Account deposit amount of this agreement |
| owedDeposit | uint256 | Account owed deposit amount of this agreement |

