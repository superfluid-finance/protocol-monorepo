# Solidity API

## SETHProxy

_Super ETH (SETH) custom super token implementation_

### TokenUpgraded

```solidity
event TokenUpgraded(address account, uint256 amount)
```

### TokenDowngraded

```solidity
event TokenDowngraded(address account, uint256 amount)
```

### receive

```solidity
receive() external payable
```

_Fallback function that delegates calls to the address returned by &#x60;_implementation()&#x60;. Will run if call data
is empty._

### upgradeByETH

```solidity
function upgradeByETH() external payable
```

### upgradeByETHTo

```solidity
function upgradeByETHTo(address to) external payable
```

### downgradeToETH

```solidity
function downgradeToETH(uint256 wad) external
```

