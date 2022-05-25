# Solidity API

## MultiFlowTesterApp

_A super app that can split incoming flows to multiple outgoing flows.
     This is used for testing CFA callbacks logic._

### ReceiverData

```solidity
struct ReceiverData {
  address to;
  uint256 proportion;
}
```

### Configuration

```solidity
struct Configuration {
  uint8 ratioPct;
  struct MultiFlowTesterApp.ReceiverData[] receivers;
}
```

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid) public
```

### _parseUserData

```solidity
function _parseUserData(bytes userData) private pure returns (address sender, struct MultiFlowTesterApp.Configuration configuration)
```

### _sumProportions

```solidity
function _sumProportions(struct MultiFlowTesterApp.ReceiverData[] receivers) internal pure returns (uint256 sum)
```

### _updateMultiFlow

```solidity
function _updateMultiFlow(struct MultiFlowTesterApp.Configuration configuration, contract ISuperToken superToken, bytes4 selector, int96 flowRate, uint256 appAllowanceGranted, bytes ctx) private returns (bytes newCtx)
```

### createFlow

```solidity
function createFlow(contract ISuperToken superToken, address receiver, int96 flowRate, bytes ctx) external returns (bytes newCtx)
```

### StackVars

```solidity
struct StackVars {
  struct ISuperfluid.Context context;
  address mfaSender;
  struct MultiFlowTesterApp.Configuration configuration;
  address flowSender;
  address flowReceiver;
}
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes, bytes ctx) external returns (bytes newCtx)
```

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes, bytes) external view returns (bytes cbdata)
```

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes, bytes ctx) external returns (bytes newCtx)
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken superToken, address agreementClass, bytes32, bytes agreementData, bytes, bytes ctx) external returns (bytes newCtx)
```

### onlyHost

```solidity
modifier onlyHost()
```

