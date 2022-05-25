# Solidity API

## TokenCustodian

_Contract which takes custody of funds which couldn&#x27;t be sent to the designated recipient_

### _ERC1820_REGISTRY

```solidity
contract IERC1820Registry _ERC1820_REGISTRY
```

### balances

```solidity
mapping(contract IERC777 &#x3D;&gt; mapping(address &#x3D;&gt; uint256)) balances
```

### constructor

```solidity
constructor() public
```

### CustodianDeposit

```solidity
event CustodianDeposit(contract IERC777 token, address recipient, uint256 amount)
```

### CustodianWithdrawal

```solidity
event CustodianWithdrawal(contract IERC777 token, address recipient, uint256 amount)
```

### flush

```solidity
function flush(contract IERC777 token, address recipient) public
```

### tokensReceived

```solidity
function tokensReceived(address, address, address, uint256 amount, bytes userData, bytes) external
```

