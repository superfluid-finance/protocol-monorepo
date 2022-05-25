# Solidity API

## SlotsBitmapLibrary

_A library implements slots bitmap on Superfluid Token storage
NOTE:
- A slots bitmap allows you to iterate through a list of data efficiently.
- A data slot can be enabled or disabled with the help of bitmap.
- MAX_NUM_SLOTS is 256 in this implementation (using one uint256)
- Superfluid token storage usage:
  - getAgreementStateSlot(bitmapStateSlotId) stores the bitmap of enabled data slots
  - getAgreementStateSlot(dataStateSlotIDStart + stotId) stores the data of the slot_

### _MAX_NUM_SLOTS

```solidity
uint32 _MAX_NUM_SLOTS
```

### findEmptySlotAndFill

```solidity
function findEmptySlotAndFill(contract ISuperfluidToken token, address account, uint256 bitmapStateSlotId, uint256 dataStateSlotIDStart, bytes32 data) public returns (uint32 slotId)
```

### clearSlot

```solidity
function clearSlot(contract ISuperfluidToken token, address account, uint256 bitmapStateSlotId, uint32 slotId) public
```

### listData

```solidity
function listData(contract ISuperfluidToken token, address account, uint256 bitmapStateSlotId, uint256 dataStateSlotIDStart) public view returns (uint32[] slotIds, bytes32[] dataList)
```

