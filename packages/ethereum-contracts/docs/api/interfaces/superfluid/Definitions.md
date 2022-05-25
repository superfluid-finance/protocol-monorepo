# SuperAppDefinitions

## Functions

### getAppLevel

```solidity
function getAppLevel(
    uint256 configWord
) internal returns (uint8)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 |  |

### isAppJailed

```solidity
function isAppJailed(
    uint256 configWord
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 |  |

### isConfigWordClean

```solidity
function isConfigWordClean(
    uint256 configWord
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 |  |

# ContextDefinitions

## Functions

### decodeCallInfo

```solidity
function decodeCallInfo(
    uint256 callInfo
) internal returns (uint8 appLevel, uint8 callType)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `callInfo` | uint256 |  |

### encodeCallInfo

```solidity
function encodeCallInfo(
    uint8 appLevel,
    uint8 callType
) internal returns (uint256 callInfo)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appLevel` | uint8 |  |
| `callType` | uint8 |  |

# FlowOperatorDefinitions

## Functions

### isPermissionsClean

```solidity
function isPermissionsClean(
    uint8 permissions
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `permissions` | uint8 |  |

# BatchOperation

# SuperfluidGovernanceConfigs

## Functions

### getTrustedForwarderConfigKey

```solidity
function getTrustedForwarderConfigKey(
    address forwarder
) internal returns (bytes32)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `forwarder` | address |  |

### getAppRegistrationConfigKey

```solidity
function getAppRegistrationConfigKey(
    address deployer,
    string registrationKey
) internal returns (bytes32)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `deployer` | address |  |
| `registrationKey` | string |  |

### getAppFactoryConfigKey

```solidity
function getAppFactoryConfigKey(
    address factory
) internal returns (bytes32)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `factory` | address |  |

### decodePPPConfig

```solidity
function decodePPPConfig(
    uint256 pppConfig
) internal returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `pppConfig` | uint256 |  |

