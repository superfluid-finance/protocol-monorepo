# Solidity API

## CFAv1Library

_for working with the constant flow agreement within solidity
the first set of functions are each for callAgreement()
the second set of functions are each for use in callAgreementWithContext()_

### InitData

```solidity
struct InitData {
  contract ISuperfluid host;
  contract IConstantFlowAgreementV1 cfa;
}
```

### createFlow

```solidity
function createFlow(struct CFAv1Library.InitData cfaLibrary, address receiver, contract ISuperfluidToken token, int96 flowRate) internal
```

_Create flow without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### createFlow

```solidity
function createFlow(struct CFAv1Library.InitData cfaLibrary, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal
```

_Create flow with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### updateFlow

```solidity
function updateFlow(struct CFAv1Library.InitData cfaLibrary, address receiver, contract ISuperfluidToken token, int96 flowRate) internal
```

_Update flow without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### updateFlow

```solidity
function updateFlow(struct CFAv1Library.InitData cfaLibrary, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal
```

_Update flow with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### deleteFlow

```solidity
function deleteFlow(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token) internal
```

_Delete flow without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |

### deleteFlow

```solidity
function deleteFlow(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, bytes userData) internal
```

_Delete flow with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| userData | bytes | The user provided data |

### createFlowWithCtx

```solidity
function createFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Create flow with context and userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### createFlowWithCtx

```solidity
function createFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Create flow with context and userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### updateFlowWithCtx

```solidity
function updateFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Update flow with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### updateFlowWithCtx

```solidity
function updateFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Update flow with context and userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### deleteFlowWithCtx

```solidity
function deleteFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Delete flow with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |

### deleteFlowWithCtx

```solidity
function deleteFlowWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, bytes userData) internal returns (bytes newCtx)
```

_Delete flow with context and userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| userData | bytes | The user provided data |

### createFlowByOperator

```solidity
function createFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Creates flow as an operator without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### createFlowByOperator

```solidity
function createFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Creates flow as an operator with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### createFlowByOperatorWithCtx

```solidity
function createFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Creates flow as an operator without userData with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### createFlowByOperatorWithCtx

```solidity
function createFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Creates flow as an operator with userData and context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### updateFlowByOperator

```solidity
function updateFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Updates a flow as an operator without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### updateFlowByOperator

```solidity
function updateFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Updates flow as an operator with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### updateFlowByOperatorWithCtx

```solidity
function updateFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) internal returns (bytes newCtx)
```

_Updates a flow as an operator without userData with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |

### updateFlowByOperatorWithCtx

```solidity
function updateFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, int96 flowRate, bytes userData) internal returns (bytes newCtx)
```

_Updates flow as an operator with userData and context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| flowRate | int96 | The desired flowRate |
| userData | bytes | The user provided data |

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Deletes a flow as an operator without userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(struct CFAv1Library.InitData cfaLibrary, address sender, address receiver, contract ISuperfluidToken token, bytes userData) internal returns (bytes newCtx)
```

_Deletes a flow as an operator with userData_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| userData | bytes | The user provided data |

### deleteFlowByOperatorWithCtx

```solidity
function deleteFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Deletes a flow as an operator without userData with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |

### deleteFlowByOperatorWithCtx

```solidity
function deleteFlowByOperatorWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address sender, address receiver, contract ISuperfluidToken token, bytes userData) internal returns (bytes newCtx)
```

_Deletes a flow as an operator with userData and context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| sender | address | The sender of the flow |
| receiver | address | The receiver of the flow |
| token | contract ISuperfluidToken | The token to flow |
| userData | bytes | The user provided data |

### updateFlowOperatorPermissions

```solidity
function updateFlowOperatorPermissions(struct CFAv1Library.InitData cfaLibrary, address flowOperator, contract ISuperfluidToken token, uint8 permissions, int96 flowRateAllowance) internal returns (bytes newCtx)
```

_Updates the permissions of a flow operator_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |
| permissions | uint8 | The number of the permissions: create &#x3D; 1; update &#x3D; 2; delete &#x3D; 4; To give multiple permissions, sum the above. create_delete &#x3D; 5; create_update_delete &#x3D; 7; etc |
| flowRateAllowance | int96 | The allowance for flow creation. Decremented as flowRate increases |

### updateFlowOperatorPermissionsWithCtx

```solidity
function updateFlowOperatorPermissionsWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address flowOperator, contract ISuperfluidToken token, uint8 permissions, int96 flowRateAllowance) internal returns (bytes newCtx)
```

_Updates the permissions of a flow operator with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |
| permissions | uint8 | The number of the permissions: create &#x3D; 1; update &#x3D; 2; delete &#x3D; 4; To give multiple permissions, sum the above. create_delete &#x3D; 5; create_update_delete &#x3D; 7; etc |
| flowRateAllowance | int96 | The allowance for flow creation. Decremented as flowRate increases |

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(struct CFAv1Library.InitData cfaLibrary, address flowOperator, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Grants full, unlimited permission to a flow operator_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |

### authorizeFlowOperatorWithFullControlWithCtx

```solidity
function authorizeFlowOperatorWithFullControlWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address flowOperator, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Grants full, unlimited permission to a flow operator with context_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |

### revokeFlowOperatorWithFullControl

```solidity
function revokeFlowOperatorWithFullControl(struct CFAv1Library.InitData cfaLibrary, address flowOperator, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Revokes all permissions from a flow operator_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |

### revokeFlowOperatorWithFullControlWithCtx

```solidity
function revokeFlowOperatorWithFullControlWithCtx(struct CFAv1Library.InitData cfaLibrary, bytes ctx, address flowOperator, contract ISuperfluidToken token) internal returns (bytes newCtx)
```

_Revokes all permissions from a flow operator_

| Name | Type | Description |
| ---- | ---- | ----------- |
| cfaLibrary | struct CFAv1Library.InitData | The cfaLibrary storage variable |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |
| flowOperator | address | The operator that can create/update/delete flows |
| token | contract ISuperfluidToken | The token of flows handled by the operator |

