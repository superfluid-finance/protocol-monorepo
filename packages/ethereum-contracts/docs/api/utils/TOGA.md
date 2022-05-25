# ITOGAv1

TOGA is a simple implementation of a continuous auction.
     It's used to designate PICs (Patrician In Charge) - a role defined per Super Token.
     Anybody can become the PIC for a Super Token by staking the highest bond (denominated in the token).
     Staking is done by simply using ERC777.send(), transferring the bond amount to be staked to this contract.
     Via userData parameter (abi-encoded int96), an exitRate can be defined. If omitted, a default will be chosen.
     The exitRate is the flowrate at which the bond is streamed back to the PIC.
     Any rewards accrued by this contract (in general the whole token balance) become part of the bond.
     When a PIC is outbid, the current bond is transferred to it with ERC777.send().

     changes in v2:
     In case that send() fails (e.g. due to a reverting hook), the bond is transferred to a custodian contract.
     Funds accumulated there can be withdrawn from there at any time.
     The current PIC can increase its bond by sending more funds using ERC777.send().

## Functions

### getCurrentPIC

```solidity
function getCurrentPIC(
    contract ISuperToken token
) external returns (address pic)
```

get the address of the current PIC for the given token.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get the PIC |

### getCurrentPICInfo

```solidity
function getCurrentPICInfo(
    contract ISuperToken token
) external returns (address pic, uint256 bond, int96 exitRate)
```

get info about the state - most importantly the bond amount - of the current PIC for the given token.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get PIC info
Notes:
The bond changes dynamically and can both grow or shrink between 2 blocks.
Even the PIC itself could change anytime, this being a continuous auction. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `pic` | address | Address of the current PIC. Returns the ZERO address if not set |
| `bond` | uint256 | The current bond amount. Can shrink or grow over time, depending on exitRate and rewards accrued |
| `exitRate` | int96 | The current flowrate of given tokens from the contract to the PIC |

### getDefaultExitRateFor

```solidity
function getDefaultExitRateFor(
    contract ISuperToken token,
    uint256 bondAmount
) external returns (int96 exitRate)
```

Get the exit rate set by default for the given token and bond amount

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get info |
| `bondAmount` | uint256 | The bond amount for which to make the calculation |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exitRate` | int96 | The exit rate set by default for a bid with the given bond amount for the given token |

### getMaxExitRateFor

```solidity
function getMaxExitRateFor(
    contract ISuperToken token,
    uint256 bondAmount
) external returns (int96 exitRate)
```

Get the max exit which can be set for the given token and bond amount

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get info |
| `bondAmount` | uint256 | The bond amount for which to calculate the max exit rate |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exitRate` | int96 | The max exit rate which can be set for the given bond amount and token

This limit is enforced only at the time of setting or updating the flow from the contract to the PIC. |

### changeExitRate

```solidity
function changeExitRate(
    contract ISuperToken token,
    int96 newExitRate
) external
```

allows the current PIC for the given token to change the exit rate

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The Super Token the exit rate should be changed for |
| `newExitRate` | int96 | The new exit rate. The same constraints as during bidding apply.

Notes:
newExitRate can't be higher than the value returned by getMaxExitRateFor() for the given token and bond.
newExitRate can also be 0, this triggers closing of the flow from the contract to the PIC.
If newExitRate is > 0 and no flow exists, a flow is created. |

## Events

### NewPIC

```solidity
event NewPIC(
    contract ISuperToken token,
    address pic,
    uint256 bond,
    int96 exitRate
)
```

Emitted on a successful bid designating a PIC

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The Super token the new PIC bid for |
| `pic` | address | The address of the new PIC |
| `bond` | uint256 | The size (amount) of the bond staked by the PIC |
| `exitRate` | int96 | The flowrate at which the bond and accrued rewards will be streamed to the PIC
The exitRate must be greater or equal zero and respect the upper bound defined by getMaxExitRateFor() |
### ExitRateChanged

```solidity
event ExitRateChanged(
    contract ISuperToken token,
    int96 exitRate
)
```

Emitted if a PIC changes the exit rate

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The Super token for which the exit rate was changed |
| `exitRate` | int96 | The new flowrate of the given token from the contract to the PIC |

# ITOGAv2

## Functions

### withdrawFundsInCustody

```solidity
function withdrawFundsInCustody(
    contract ISuperToken token
) external
```

allows previous PICs to withdraw bonds which couldn't be sent back to them

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to withdraw funds in custody |

## Events

### BondIncreased

```solidity
event BondIncreased(
    contract ISuperToken token,
    uint256 additionalBond
)
```

Emitted if a PIC increases its bond

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |
| `additionalBond` | uint256 | The additional amount added to the bond |

# TOGA

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host_,
    uint256 minBondDuration_,
    contract TokenCustodian custodian_
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host_` | contract ISuperfluid |  |
| `minBondDuration_` | uint256 |  |
| `custodian_` | contract TokenCustodian |  |

### getCurrentPIC

```solidity
function getCurrentPIC(
    contract ISuperToken token
) external returns (address pic)
```

get the address of the current PIC for the given token.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get the PIC |

### getCurrentPICInfo

```solidity
function getCurrentPICInfo(
    contract ISuperToken token
) external returns (address pic, uint256 bond, int96 exitRate)
```

get info about the state - most importantly the bond amount - of the current PIC for the given token.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to get PIC info
Notes:
The bond changes dynamically and can both grow or shrink between 2 blocks.
Even the PIC itself could change anytime, this being a continuous auction. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `pic` | address | Address of the current PIC. Returns the ZERO address if not set |
| `bond` | uint256 | The current bond amount. Can shrink or grow over time, depending on exitRate and rewards accrued |
| `exitRate` | int96 | The current flowrate of given tokens from the contract to the PIC |

### capToInt96

```solidity
function capToInt96(
    int256 value
) internal returns (int96)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `value` | int256 |  |

### getDefaultExitRateFor

```solidity
function getDefaultExitRateFor(
    contract ISuperToken ,
    uint256 bondAmount
) public returns (int96 exitRate)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `bondAmount` | uint256 |  |

### getMaxExitRateFor

```solidity
function getMaxExitRateFor(
    contract ISuperToken ,
    uint256 bondAmount
) external returns (int96 exitRate)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `bondAmount` | uint256 |  |

### changeExitRate

```solidity
function changeExitRate(
    contract ISuperToken token,
    int96 newExitRate
) external
```

allows the current PIC for the given token to change the exit rate

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The Super Token the exit rate should be changed for |
| `newExitRate` | int96 | The new exit rate. The same constraints as during bidding apply.

Notes:
newExitRate can't be higher than the value returned by getMaxExitRateFor() for the given token and bond.
newExitRate can also be 0, this triggers closing of the flow from the contract to the PIC.
If newExitRate is > 0 and no flow exists, a flow is created. |

### withdrawFundsInCustody

```solidity
function withdrawFundsInCustody(
    contract ISuperToken token
) external
```

allows previous PICs to withdraw bonds which couldn't be sent back to them

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | The token for which to withdraw funds in custody |

### _getCurrentPICBond

```solidity
function _getCurrentPICBond(
    contract ISuperToken token
) internal returns (uint256 bond)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

### _becomePIC

```solidity
function _becomePIC(
    contract ISuperToken token,
    address newPIC,
    uint256 amount,
    int96 exitRate
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |
| `newPIC` | address |  |
| `amount` | uint256 |  |
| `exitRate` | int96 |  |

### tokensReceived

```solidity
function tokensReceived(
    address ,
    address from,
    address ,
    uint256 amount,
    bytes userData,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | address |  |
| `from` | address |  |
| `` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `` | bytes |  |

