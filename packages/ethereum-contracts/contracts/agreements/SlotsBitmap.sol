// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperfluidToken
} from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";

library SlotsBitmap {

    uint32 internal constant _MAX_NUM_SLOTS = 256;

    function findEmptySlotAndFill(
        ISuperfluidToken token,
        address account,
        uint256 bitmapStateSlotId,
        bytes32 data
    )
        public
        returns (uint32 slotId)
    {
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            account,
            bitmapStateSlotId, 1)[0]);
        for (slotId = 0; slotId < _MAX_NUM_SLOTS; ++slotId) {
            if ((uint256(subsBitmap >> slotId) & 1) == 0) {
                // update slot data
                bytes32[] memory slotData = new bytes32[](1);
                slotData[0] = data;
                token.updateAgreementStateSlot(
                    account,
                    bitmapStateSlotId + slotId,
                    slotData);
                // update slot map
                slotData[0] = bytes32(subsBitmap | (1 << uint256(slotId)));
                token.updateAgreementStateSlot(
                    account,
                    bitmapStateSlotId,
                    slotData);
                // update the slots
                break;
            }
        }
    }

    function clearSlot(
        ISuperfluidToken token,
        address account,
        uint256 bitmapStateSlotId,
        uint32 slotId
    )
        public
    {
        uint256 subsBitmap = uint256(token.getAgreementStateSlot(
            address(this),
            account,
            bitmapStateSlotId, 1)[0]);
        bytes32[] memory slotData = new bytes32[](1);
        slotData[0] = bytes32(subsBitmap & ~(1 << uint256(slotId)));
        // zero the data
        token.updateAgreementStateSlot(
            account,
            bitmapStateSlotId,
            slotData);
    }

    function listData(
       ISuperfluidToken token,
       address account,
       uint256 bitmapStateSlotId
    )
       public view
       returns (bytes32[] memory dataList)
    {
       uint256 subsBitmap = uint256(token.getAgreementStateSlot(
           address(this),
           account,
           bitmapStateSlotId, 1)[0]);

       dataList = new bytes32[](_MAX_NUM_SLOTS);
       // read all slots
       uint nSlots;
       for (uint32 slotId = 0; slotId < _MAX_NUM_SLOTS; ++slotId) {
           if ((uint256(subsBitmap >> slotId) & 1) == 0) continue;
           dataList[nSlots++] = token.getAgreementStateSlot(
               address(this),
               account,
               bitmapStateSlotId + slotId, 1)[0];
       }
       // resize memory arrays
       assembly {
           mstore(dataList, nSlots)
       }
    }
}
