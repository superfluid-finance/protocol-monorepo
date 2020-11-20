// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

/**
 * @dev Super app definitions library
 */
library SuperAppDefinitions {

    /**************************************************************************
    / App manifest config word
    /**************************************************************************/
    uint256 constant internal TYPE_APP_FINAL = 1 << 0;
    uint256 constant internal TYPE_APP_SECOND = 1 << 1;
    uint256 constant internal JAIL = 1 << 15;

    /**************************************************************************
    / Callback implementation bit masks
    /**************************************************************************/
    uint256 constant internal AGREEMENT_CALLBACK_NOOP_BITMASKS = 0xFF << 32;
    uint256 constant internal BEFORE_AGREEMENT_CREATED_NOOP = 1 << (32 + 0);
    uint256 constant internal AFTER_AGREEMENT_CREATED_NOOP = 1 << (32 + 1);
    uint256 constant internal BEFORE_AGREEMENT_UPDATED_NOOP = 1 << (32 + 2);
    uint256 constant internal AFTER_AGREEMENT_UPDATED_NOOP = 1 << (32 + 3);
    uint256 constant internal BEFORE_AGREEMENT_TERMINATED_NOOP = 1 << (32 + 4);
    uint256 constant internal AFTER_AGREEMENT_TERMINATED_NOOP = 1 << (32 + 5);

}

library ContextDefinitions {

    /**************************************************************************
    / Call info
    /**************************************************************************/

    // app level
    uint256 constant internal CALL_INFO_APP_CALLBACK_LEVEL_MASK = 0xFF;

    // call type
    uint8 constant internal CALL_INFO_CALL_TYPE_APP_ACTION = 1;
    uint8 constant internal CALL_INFO_CALL_TYPE_APP_CALLBACK = 2;
    uint256 constant internal CALL_INFO_CALL_TYPE_SHIFT = 32;
    uint256 constant internal CALL_INFO_CALL_TYPE_MASK = 0xF << CALL_INFO_CALL_TYPE_SHIFT;
    uint256 constant internal CALL_INFO_CALL_TYPE_APP_ACTION_BIT =
        uint256(CALL_INFO_CALL_TYPE_APP_ACTION) << CALL_INFO_CALL_TYPE_SHIFT;
    uint256 constant internal CALL_INFO_CALL_TYPE_APP_CALLBACK_BIT =
        uint256(CALL_INFO_CALL_TYPE_APP_CALLBACK) << CALL_INFO_CALL_TYPE_SHIFT;

    function decodeCallInfo(uint256 callInfo)
        internal pure
        returns (uint8 cbLevel, uint8 callType)
    {
        cbLevel = uint8(callInfo & CALL_INFO_APP_CALLBACK_LEVEL_MASK);
        callType = uint8((callInfo & CALL_INFO_CALL_TYPE_MASK) >> CALL_INFO_CALL_TYPE_SHIFT);
    }

    function encodeCallInfo(uint8 cbLevel, uint8 callType)
        internal pure
        returns (uint256 callInfo)
    {
        return uint256(cbLevel) |
            (uint256(callType) << CALL_INFO_CALL_TYPE_SHIFT);
    }

}
