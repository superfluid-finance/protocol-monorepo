# Solidity API

## ForwarderMock

_A test forwarder that can impersonate any account needed.

It is obviously not secure for any production use._

### ForwardRequest

```solidity
struct ForwardRequest {
  address from;
  address to;
  uint256 value;
  uint256 gas;
  bytes data;
}
```

### execute

```solidity
function execute(struct ForwarderMock.ForwardRequest req) external payable
```

