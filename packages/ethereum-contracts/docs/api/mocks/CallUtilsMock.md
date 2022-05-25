# Solidity API

## CallUtilsMock

### Custom

```solidity
error Custom()
```

### CustomVal

```solidity
error CustomVal(uint256)
```

### Gm

```solidity
enum Gm {
  Ngmi
}
```

### a

```solidity
uint256[] a
```

### revertEmpty

```solidity
function revertEmpty() public pure
```

### revertAssert

```solidity
function revertAssert() public pure
```

### revertOverflow

```solidity
function revertOverflow() public pure returns (uint256)
```

### revertDivByZero

```solidity
function revertDivByZero() public pure
```

### revertEnum

```solidity
function revertEnum() public pure
```

### revertPop

```solidity
function revertPop() public
```

### revertArrayAccess

```solidity
function revertArrayAccess() public view
```

### revertBigArray

```solidity
function revertBigArray() public pure returns (uint8[])
```

### revertZeroInitializedFunctionPointer

```solidity
function revertZeroInitializedFunctionPointer() external pure returns (int256)
```

### revertString

```solidity
function revertString() public pure
```

### revertCustom

```solidity
function revertCustom() public pure
```

### revertCustomVal

```solidity
function revertCustomVal() public pure
```

### revertTest

```solidity
function revertTest(string funcSig) public
```

