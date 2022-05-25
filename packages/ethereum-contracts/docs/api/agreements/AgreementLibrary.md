# Solidity API

## AgreementLibrary

_Helper library for building super agreement_

### authorizeTokenAccess

```solidity
function authorizeTokenAccess(contract ISuperfluidToken token, bytes ctx) internal view returns (struct ISuperfluid.Context)
```

_Authorize the msg.sender to access token agreement storage

NOTE:
- msg.sender must be the expected host contract.
- it should revert on unauthorized access._

### CallbackInputs

```solidity
struct CallbackInputs {
  contract ISuperfluidToken token;
  address account;
  bytes32 agreementId;
  bytes agreementData;
  uint256 appAllowanceGranted;
  int256 appAllowanceUsed;
  uint256 noopBit;
}
```

### createCallbackInputs

```solidity
function createCallbackInputs(contract ISuperfluidToken token, address account, bytes32 agreementId, bytes agreementData) internal pure returns (struct AgreementLibrary.CallbackInputs inputs)
```

### callAppBeforeCallback

```solidity
function callAppBeforeCallback(struct AgreementLibrary.CallbackInputs inputs, bytes ctx) internal returns (bytes cbdata)
```

### callAppAfterCallback

```solidity
function callAppAfterCallback(struct AgreementLibrary.CallbackInputs inputs, bytes cbdata, bytes ctx) internal returns (struct ISuperfluid.Context appContext, bytes newCtx)
```

### _selectorFromNoopBit

```solidity
function _selectorFromNoopBit(uint256 noopBit) private pure returns (bytes4 selector)
```

### _pushCallbackStack

```solidity
function _pushCallbackStack(bytes ctx, struct AgreementLibrary.CallbackInputs inputs) private returns (bytes appCtx)
```

### _popCallbackStack

```solidity
function _popCallbackStack(bytes ctx, int256 appAllowanceUsedDelta) private returns (bytes newCtx)
```

### max

```solidity
function max(int256 a, int256 b) internal pure returns (int256)
```

### max

```solidity
function max(uint256 a, uint256 b) internal pure returns (uint256)
```

### min

```solidity
function min(int256 a, int256 b) internal pure returns (int256)
```

