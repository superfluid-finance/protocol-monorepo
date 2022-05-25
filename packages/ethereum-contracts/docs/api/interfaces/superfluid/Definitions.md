# Solidity API

## SuperAppDefinitions

### APP_LEVEL_MASK

```solidity
uint256 APP_LEVEL_MASK
```

### APP_LEVEL_FINAL

```solidity
uint256 APP_LEVEL_FINAL
```

### APP_LEVEL_SECOND

```solidity
uint256 APP_LEVEL_SECOND
```

### getAppLevel

```solidity
function getAppLevel(uint256 configWord) internal pure returns (uint8)
```

### APP_JAIL_BIT

```solidity
uint256 APP_JAIL_BIT
```

### isAppJailed

```solidity
function isAppJailed(uint256 configWord) internal pure returns (bool)
```

### AGREEMENT_CALLBACK_NOOP_BITMASKS

```solidity
uint256 AGREEMENT_CALLBACK_NOOP_BITMASKS
```

### BEFORE_AGREEMENT_CREATED_NOOP

```solidity
uint256 BEFORE_AGREEMENT_CREATED_NOOP
```

### AFTER_AGREEMENT_CREATED_NOOP

```solidity
uint256 AFTER_AGREEMENT_CREATED_NOOP
```

### BEFORE_AGREEMENT_UPDATED_NOOP

```solidity
uint256 BEFORE_AGREEMENT_UPDATED_NOOP
```

### AFTER_AGREEMENT_UPDATED_NOOP

```solidity
uint256 AFTER_AGREEMENT_UPDATED_NOOP
```

### BEFORE_AGREEMENT_TERMINATED_NOOP

```solidity
uint256 BEFORE_AGREEMENT_TERMINATED_NOOP
```

### AFTER_AGREEMENT_TERMINATED_NOOP

```solidity
uint256 AFTER_AGREEMENT_TERMINATED_NOOP
```

### APP_RULE_REGISTRATION_ONLY_IN_CONSTRUCTOR

```solidity
uint256 APP_RULE_REGISTRATION_ONLY_IN_CONSTRUCTOR
```

### APP_RULE_NO_REGISTRATION_FOR_EOA

```solidity
uint256 APP_RULE_NO_REGISTRATION_FOR_EOA
```

### APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK

```solidity
uint256 APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
```

### APP_RULE_NO_CRITICAL_SENDER_ACCOUNT

```solidity
uint256 APP_RULE_NO_CRITICAL_SENDER_ACCOUNT
```

### APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT

```solidity
uint256 APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT
```

### APP_RULE_CTX_IS_READONLY

```solidity
uint256 APP_RULE_CTX_IS_READONLY
```

### APP_RULE_CTX_IS_NOT_CLEAN

```solidity
uint256 APP_RULE_CTX_IS_NOT_CLEAN
```

### APP_RULE_CTX_IS_MALFORMATED

```solidity
uint256 APP_RULE_CTX_IS_MALFORMATED
```

### APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED

```solidity
uint256 APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED
```

### APP_RULE_COMPOSITE_APP_IS_JAILED

```solidity
uint256 APP_RULE_COMPOSITE_APP_IS_JAILED
```

### APP_RULE_MAX_APP_LEVEL_REACHED

```solidity
uint256 APP_RULE_MAX_APP_LEVEL_REACHED
```

### isConfigWordClean

```solidity
function isConfigWordClean(uint256 configWord) internal pure returns (bool)
```

## ContextDefinitions

### CALL_INFO_APP_LEVEL_MASK

```solidity
uint256 CALL_INFO_APP_LEVEL_MASK
```

### CALL_INFO_CALL_TYPE_SHIFT

```solidity
uint256 CALL_INFO_CALL_TYPE_SHIFT
```

### CALL_INFO_CALL_TYPE_MASK

```solidity
uint256 CALL_INFO_CALL_TYPE_MASK
```

### CALL_INFO_CALL_TYPE_AGREEMENT

```solidity
uint8 CALL_INFO_CALL_TYPE_AGREEMENT
```

### CALL_INFO_CALL_TYPE_APP_ACTION

```solidity
uint8 CALL_INFO_CALL_TYPE_APP_ACTION
```

### CALL_INFO_CALL_TYPE_APP_CALLBACK

```solidity
uint8 CALL_INFO_CALL_TYPE_APP_CALLBACK
```

### decodeCallInfo

```solidity
function decodeCallInfo(uint256 callInfo) internal pure returns (uint8 appLevel, uint8 callType)
```

### encodeCallInfo

```solidity
function encodeCallInfo(uint8 appLevel, uint8 callType) internal pure returns (uint256 callInfo)
```

## FlowOperatorDefinitions

### AUTHORIZE_FLOW_OPERATOR_CREATE

```solidity
uint8 AUTHORIZE_FLOW_OPERATOR_CREATE
```

### AUTHORIZE_FLOW_OPERATOR_UPDATE

```solidity
uint8 AUTHORIZE_FLOW_OPERATOR_UPDATE
```

### AUTHORIZE_FLOW_OPERATOR_DELETE

```solidity
uint8 AUTHORIZE_FLOW_OPERATOR_DELETE
```

### AUTHORIZE_FULL_CONTROL

```solidity
uint8 AUTHORIZE_FULL_CONTROL
```

### REVOKE_FLOW_OPERATOR_CREATE

```solidity
uint8 REVOKE_FLOW_OPERATOR_CREATE
```

### REVOKE_FLOW_OPERATOR_UPDATE

```solidity
uint8 REVOKE_FLOW_OPERATOR_UPDATE
```

### REVOKE_FLOW_OPERATOR_DELETE

```solidity
uint8 REVOKE_FLOW_OPERATOR_DELETE
```

### isPermissionsClean

```solidity
function isPermissionsClean(uint8 permissions) internal pure returns (bool)
```

## BatchOperation

### OPERATION_TYPE_ERC20_APPROVE

```solidity
uint32 OPERATION_TYPE_ERC20_APPROVE
```

_ERC20.approve batch operation type

Call spec:
ISuperToken(target).operationApprove(
    abi.decode(data, (address spender, uint256 amount))
)_

### OPERATION_TYPE_ERC20_TRANSFER_FROM

```solidity
uint32 OPERATION_TYPE_ERC20_TRANSFER_FROM
```

_ERC20.transferFrom batch operation type

Call spec:
ISuperToken(target).operationTransferFrom(
    abi.decode(data, (address sender, address recipient, uint256 amount)
)_

### OPERATION_TYPE_SUPERTOKEN_UPGRADE

```solidity
uint32 OPERATION_TYPE_SUPERTOKEN_UPGRADE
```

_SuperToken.upgrade batch operation type

Call spec:
ISuperToken(target).operationUpgrade(
    abi.decode(data, (uint256 amount)
)_

### OPERATION_TYPE_SUPERTOKEN_DOWNGRADE

```solidity
uint32 OPERATION_TYPE_SUPERTOKEN_DOWNGRADE
```

_SuperToken.downgrade batch operation type

Call spec:
ISuperToken(target).operationDowngrade(
    abi.decode(data, (uint256 amount)
)_

### OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT

```solidity
uint32 OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT
```

_Superfluid.callAgreement batch operation type

Call spec:
callAgreement(
    ISuperAgreement(target)),
    abi.decode(data, (bytes calldata, bytes userdata)
)_

### OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION

```solidity
uint32 OPERATION_TYPE_SUPERFLUID_CALL_APP_ACTION
```

_Superfluid.callAppAction batch operation type

Call spec:
callAppAction(
    ISuperApp(target)),
    data
)_

## SuperfluidGovernanceConfigs

### SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY

```solidity
bytes32 SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY
```

### CFAV1_PPP_CONFIG_KEY

```solidity
bytes32 CFAV1_PPP_CONFIG_KEY
```

### SUPERTOKEN_MINIMUM_DEPOSIT_KEY

```solidity
bytes32 SUPERTOKEN_MINIMUM_DEPOSIT_KEY
```

### getTrustedForwarderConfigKey

```solidity
function getTrustedForwarderConfigKey(address forwarder) internal pure returns (bytes32)
```

### getAppRegistrationConfigKey

```solidity
function getAppRegistrationConfigKey(address deployer, string registrationKey) internal pure returns (bytes32)
```

### getAppFactoryConfigKey

```solidity
function getAppFactoryConfigKey(address factory) internal pure returns (bytes32)
```

### decodePPPConfig

```solidity
function decodePPPConfig(uint256 pppConfig) internal pure returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

