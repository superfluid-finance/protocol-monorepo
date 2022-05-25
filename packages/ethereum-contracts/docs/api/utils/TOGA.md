# Solidity API

## ITOGAv1

_TOGA is a simple implementation of a continuous auction.
     It&#x27;s used to designate PICs (Patrician In Charge) - a role defined per Super Token.
     Anybody can become the PIC for a Super Token by staking the highest bond (denominated in the token).
     Staking is done by simply using ERC777.send(), transferring the bond amount to be staked to this contract.
     Via userData parameter (abi-encoded int96), an exitRate can be defined. If omitted, a default will be chosen.
     The exitRate is the flowrate at which the bond is streamed back to the PIC.
     Any rewards accrued by this contract (in general the whole token balance) become part of the bond.
     When a PIC is outbid, the current bond is transferred to it with ERC777.send().

     changes in v2:
     In case that send() fails (e.g. due to a reverting hook), the bond is transferred to a custodian contract.
     Funds accumulated there can be withdrawn from there at any time.
     The current PIC can increase its bond by sending more funds using ERC777.send()._

### getCurrentPIC

```solidity
function getCurrentPIC(contract ISuperToken token) external view returns (address pic)
```

_get the address of the current PIC for the given token._

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get the PIC |

### getCurrentPICInfo

```solidity
function getCurrentPICInfo(contract ISuperToken token) external view returns (address pic, uint256 bond, int96 exitRate)
```

_get info about the state - most importantly the bond amount - of the current PIC for the given token._

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get PIC info Notes: The bond changes dynamically and can both grow or shrink between 2 blocks. Even the PIC itself could change anytime, this being a continuous auction. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| pic | address | Address of the current PIC. Returns the ZERO address if not set |
| bond | uint256 | The current bond amount. Can shrink or grow over time, depending on exitRate and rewards accrued |
| exitRate | int96 | The current flowrate of given tokens from the contract to the PIC |

### getDefaultExitRateFor

```solidity
function getDefaultExitRateFor(contract ISuperToken token, uint256 bondAmount) external view returns (int96 exitRate)
```

_Get the exit rate set by default for the given token and bond amount_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get info |
| bondAmount | uint256 | The bond amount for which to make the calculation |

| Name | Type | Description |
| ---- | ---- | ----------- |
| exitRate | int96 | The exit rate set by default for a bid with the given bond amount for the given token |

### getMaxExitRateFor

```solidity
function getMaxExitRateFor(contract ISuperToken token, uint256 bondAmount) external view returns (int96 exitRate)
```

_Get the max exit which can be set for the given token and bond amount_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get info |
| bondAmount | uint256 | The bond amount for which to calculate the max exit rate |

| Name | Type | Description |
| ---- | ---- | ----------- |
| exitRate | int96 | The max exit rate which can be set for the given bond amount and token This limit is enforced only at the time of setting or updating the flow from the contract to the PIC. |

### changeExitRate

```solidity
function changeExitRate(contract ISuperToken token, int96 newExitRate) external
```

_allows the current PIC for the given token to change the exit rate_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The Super Token the exit rate should be changed for |
| newExitRate | int96 | The new exit rate. The same constraints as during bidding apply. Notes: newExitRate can&#x27;t be higher than the value returned by getMaxExitRateFor() for the given token and bond. newExitRate can also be 0, this triggers closing of the flow from the contract to the PIC. If newExitRate is &gt; 0 and no flow exists, a flow is created. |

### NewPIC

```solidity
event NewPIC(contract ISuperToken token, address pic, uint256 bond, int96 exitRate)
```

_Emitted on a successful bid designating a PIC_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The Super token the new PIC bid for |
| pic | address | The address of the new PIC |
| bond | uint256 | The size (amount) of the bond staked by the PIC |
| exitRate | int96 | The flowrate at which the bond and accrued rewards will be streamed to the PIC The exitRate must be greater or equal zero and respect the upper bound defined by getMaxExitRateFor() |

### ExitRateChanged

```solidity
event ExitRateChanged(contract ISuperToken token, int96 exitRate)
```

_Emitted if a PIC changes the exit rate_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The Super token for which the exit rate was changed |
| exitRate | int96 | The new flowrate of the given token from the contract to the PIC |

## ITOGAv2

### withdrawFundsInCustody

```solidity
function withdrawFundsInCustody(contract ISuperToken token) external
```

_allows previous PICs to withdraw bonds which couldn&#x27;t be sent back to them_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to withdraw funds in custody |

### BondIncreased

```solidity
event BondIncreased(contract ISuperToken token, uint256 additionalBond)
```

_Emitted if a PIC increases its bond_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken |  |
| additionalBond | uint256 | The additional amount added to the bond |

## TOGA

### LockablePIC

```solidity
struct LockablePIC {
  address addr;
  bool lock;
}
```

### _currentPICs

```solidity
mapping(contract ISuperToken &#x3D;&gt; struct TOGA.LockablePIC) _currentPICs
```

### _host

```solidity
contract ISuperfluid _host
```

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### minBondDuration

```solidity
uint256 minBondDuration
```

### _ERC1820_REG

```solidity
contract IERC1820Registry _ERC1820_REG
```

### ERC777_SEND_GAS_LIMIT

```solidity
uint64 ERC777_SEND_GAS_LIMIT
```

### custodian

```solidity
contract TokenCustodian custodian
```

### constructor

```solidity
constructor(contract ISuperfluid host_, uint256 minBondDuration_, contract TokenCustodian custodian_) public
```

### getCurrentPIC

```solidity
function getCurrentPIC(contract ISuperToken token) external view returns (address pic)
```

_get the address of the current PIC for the given token._

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get the PIC |

### getCurrentPICInfo

```solidity
function getCurrentPICInfo(contract ISuperToken token) external view returns (address pic, uint256 bond, int96 exitRate)
```

_get info about the state - most importantly the bond amount - of the current PIC for the given token._

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to get PIC info Notes: The bond changes dynamically and can both grow or shrink between 2 blocks. Even the PIC itself could change anytime, this being a continuous auction. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| pic | address | Address of the current PIC. Returns the ZERO address if not set |
| bond | uint256 | The current bond amount. Can shrink or grow over time, depending on exitRate and rewards accrued |
| exitRate | int96 | The current flowrate of given tokens from the contract to the PIC |

### capToInt96

```solidity
function capToInt96(int256 value) internal pure returns (int96)
```

### getDefaultExitRateFor

```solidity
function getDefaultExitRateFor(contract ISuperToken, uint256 bondAmount) public view returns (int96 exitRate)
```

### getMaxExitRateFor

```solidity
function getMaxExitRateFor(contract ISuperToken, uint256 bondAmount) external view returns (int96 exitRate)
```

### changeExitRate

```solidity
function changeExitRate(contract ISuperToken token, int96 newExitRate) external
```

_allows the current PIC for the given token to change the exit rate_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The Super Token the exit rate should be changed for |
| newExitRate | int96 | The new exit rate. The same constraints as during bidding apply. Notes: newExitRate can&#x27;t be higher than the value returned by getMaxExitRateFor() for the given token and bond. newExitRate can also be 0, this triggers closing of the flow from the contract to the PIC. If newExitRate is &gt; 0 and no flow exists, a flow is created. |

### withdrawFundsInCustody

```solidity
function withdrawFundsInCustody(contract ISuperToken token) external
```

_allows previous PICs to withdraw bonds which couldn&#x27;t be sent back to them_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | The token for which to withdraw funds in custody |

### _getCurrentPICBond

```solidity
function _getCurrentPICBond(contract ISuperToken token) internal view returns (uint256 bond)
```

### _becomePIC

```solidity
function _becomePIC(contract ISuperToken token, address newPIC, uint256 amount, int96 exitRate) internal
```

### tokensReceived

```solidity
function tokensReceived(address, address from, address, uint256 amount, bytes userData, bytes) external
```

