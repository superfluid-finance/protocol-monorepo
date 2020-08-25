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

    function replaceMsgSender(
        bytes memory ctx,
        address newMsgSender
    ) internal pure returns(bytes memory newCtx, bytes32 stamp, address oldSender) {
        (uint8 level, address sender,uint64 gasReservation) = decodeContext(ctx);
        (newCtx, stamp)= encodeContext(level, newMsgSender, gasReservation);
        oldSender = sender;
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
        ctx = abi.encode(level, sender, gasReservation);
        stamp = keccak256(abi.encodePacked(ctx));
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

    function replaceContext(bytes memory data, bytes memory ctx) internal pure returns (bytes memory) {
        uint256 paddedLength = (ctx.length / 32 + 1) * 32;
        data[data.length - 2] = byte(uint8(ctx.length >> 8));
        data[data.length - 1] = byte(uint8(ctx.length));
        return abi.encodePacked(
            data,
            ctx, new bytes(paddedLength - ctx.length) /* ctx padding */
        );
    }
}
