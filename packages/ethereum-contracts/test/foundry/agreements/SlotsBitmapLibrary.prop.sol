// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import "forge-std/Test.sol";

import { SlotsBitmapLibrary } from "@superfluid-finance/ethereum-contracts/contracts/libs/SlotsBitmapLibrary.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { ISuperfluidToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidToken.sol";
import { ISuperTokenFactory } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperTokenFactory.sol";
import {
    ERC20PresetMinterPauser,
    SuperToken,
    SuperTokenFactory,
    SuperfluidFrameworkDeployer
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";

contract SlotsBitmapLibraryMock {
    /// @dev Subscriber state slot id for storing subs bitmap
    uint256 public constant _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID = 0;
    /// @dev Subscriber state slot id starting ptoint for subscription data
    uint256 public constant _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START = 1 << 128;
    // function _findAndFillSubsBitmap(
    //     ISuperfluidToken _token,
    //     address _subscriber,
    //     bytes32 iId
    // )
    //     public returns (uint32 subId)
    // {
    //     return SlotsBitmapLibrary.findEmptySlotAndFill(
    //         _token, 
    //         _subscriber,
    //         _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID,
    //         _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START,
    //         iId
    //     );
    // }

    // function _clearSubsBitmap(
    //     ISuperfluidToken _token,
    //     address _subscriber,
    //     uint32 _subId
    // ) 
    //     public 
    // {
    //     SlotsBitmapLibrary.clearSlot(
    //         _token, 
    //         _subscriber,
    //         _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID,
    //         _subId
    //     );
    // }
}

contract SlotsBitmapLibraryProperties is Test {

    ERC20PresetMinterPauser private token;
    SuperToken immutable private superToken;
    address constant subscriber = address(1);
    bytes32 constant fakeId = keccak256(abi.encodePacked("subscriber", subscriber));

    constructor() {
        (, , , SuperTokenFactory _superTokenFactory) = sfDeployer.getFramework();
        token = new ERC20PresetMinterPauser("Test Token", "TST");
        superToken = _superTokenFactory.createERC20Wrapper(
            token,
            18,
            ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
            "Test Token",
            "TSTx"
        );
    }
    
    function testSlotsBitmapLibraryMaxSlots() public {
        assertEq(SlotsBitmapLibrary._MAX_NUM_SLOTS, 256);
    }

    function testAddOneAndList() public {
        (uint32[] memory slotIds, ) = SlotsBitmapLibrary.listData(fakeToken, subscriber, 0, 1 << 128);
        assertEq(slotIds.length, 0);
        SlotsBitmapLibrary.findEmptySlotAndFill(fakeToken, subscriber, 0, 1 << 128, fakeId);

        (uint32[] memory newSlotIds, ) = SlotsBitmapLibrary.listData(fakeToken, subscriber, 0, 1 << 128);
        assertEq(newSlotIds.length, 1);
    }

    // fuzz w/ 2 params: the number of slots to add
    // a random number which is the slot which you will clear
    // loop through and add a bunch of slots
    // assert number of elements with listData
    // clear one of the slots
    // assert number of elements is one less
    // add another element
    // assert number of elements is one more
    // it will probably test adding 0 slots and trying to remove
    //
    function testSlotsBitmapLibraryBehavior(uint8 numSlots, uint8 subIdToRemove) public {
        // assume that the actual subIdToRemove is bounded between 0 and numSlots
    }
}