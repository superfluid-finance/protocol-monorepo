// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;
/*
    Implementation of Context Encoding and Decoding DATA
*/

library ContextLibrary {

    function createStamp(bytes memory ctx) internal pure returns(bytes32 stamp) {
        return keccak256(abi.encodePacked(ctx));
    }

    function validateContext(bytes memory ctx, bytes32 stamp) internal pure returns(bool) {
        return keccak256(abi.encodePacked(ctx)) == stamp;
    }

    function updateContextNewCaller(
        bytes memory ctx,
        address caller
    )
        internal
        pure
        returns(bytes memory newCtx, bytes32 stamp)
    {
        (uint8 level, ,uint64 gasReservation) = decodeContext(ctx);
        return encodeContext(level, caller, gasReservation);
    }

    function encodeContext(
        uint8 level,
        address sender,
        uint64 gasReservation
    )
        internal
        pure
        returns(bytes memory ctx, bytes32 stamp)
    {
        bytes memory newCtx = abi.encode(level, sender, gasReservation);
        return (newCtx, keccak256(abi.encodePacked(ctx)));
    }

    function decodeContext(bytes memory ctx) internal pure returns(uint8, address, uint64) {
        return abi.decode(ctx, (uint8, address, uint64));
    }

    function getCaller(bytes memory ctx) internal pure returns(address) {
        (, address caller, ) = decodeContext(ctx);
        return caller;
    }

    function splitReturnedData(
        bytes memory returnedData
    )
        internal
        pure
        returns(bytes memory newCtx, bytes memory cbdata)
    {
        return abi.decode(returnedData, (bytes, bytes));
    }
}
