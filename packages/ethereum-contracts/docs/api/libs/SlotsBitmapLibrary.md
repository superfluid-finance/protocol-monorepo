# SlotsBitmapLibrary

A library implements slots bitmap on Superfluid Token storage
NOTE:
- A slots bitmap allows you to iterate through a list of data efficiently.
- A data slot can be enabled or disabled with the help of bitmap.
- MAX_NUM_SLOTS is 256 in this implementation (using one uint256)
- Superfluid token storage usage:
  - getAgreementStateSlot(bitmapStateSlotId) stores the bitmap of enabled data slots
  - getAgreementStateSlot(dataStateSlotIDStart + stotId) stores the data of the slot

## Functions

### findEmptySlotAndFill

```solidity
function findEmptySlotAndFill(
    contract ISuperfluidToken token,
    address account,
    uint256 bitmapStateSlotId,
    uint256 dataStateSlotIDStart,
    bytes32 data
) public returns (uint32 slotId)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `bitmapStateSlotId` | uint256 |  |
| `dataStateSlotIDStart` | uint256 |  |
| `data` | bytes32 |  |

### clearSlot

```solidity
function clearSlot(
    contract ISuperfluidToken token,
    address account,
    uint256 bitmapStateSlotId,
    uint32 slotId
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `bitmapStateSlotId` | uint256 |  |
| `slotId` | uint32 |  |

### listData

```solidity
function listData(
    contract ISuperfluidToken token,
    address account,
    uint256 bitmapStateSlotId,
    uint256 dataStateSlotIDStart
) public returns (uint32[] slotIds, bytes32[] dataList)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `bitmapStateSlotId` | uint256 |  |
| `dataStateSlotIDStart` | uint256 |  |

