// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;
/*
    Implementation of Context Helper
*/

library ContextLibrary {

    /// The context structure
    struct Context {
        uint8 level;
        address msgSender;
        uint256 gasRequirement;
    }

    /**
     * @dev Encode context to ctx bytes array and a stamp of it
     */
    function encode(Context memory context) internal pure returns (bytes memory ctx, bytes32 ctxStamp) {
        ctx = abi.encode(
            context.level,
            context.msgSender,
            context.gasRequirement
        );
        ctxStamp = keccak256(abi.encodePacked(ctx));
    }

    function decode(bytes memory ctx) internal pure returns (Context memory context) {
        (
            context.level,
            context.msgSender,
            context.gasRequirement
        ) = abi.decode(ctx, (uint8, address, uint256));
    }

    function validate(bytes memory ctx, bytes32 ctxStamp) internal pure returns (bool) {
        return keccak256(abi.encodePacked(ctx)) == ctxStamp;
    }

    /*
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
    */

    /**
     * @dev Replace the placeholder ctx argument in the end of the data
     * @param data ABI encoded data, with placeholder ctx (zero bytes data) as its last argument
     * @param ctx The ctx to be replaced to
     * @return New data with replaced ctx
     */
    function replaceContext(bytes memory data, bytes memory ctx) internal pure returns (bytes memory) {
        // ctx needs to be padded to align with 32 bytes bouondary
        uint256 paddedLength = (ctx.length / 32 + 1) * 32;
        // ctx length has to be stored in the length word of placehoolder ctx
        // we support up to 2^16 length of the data
        data[data.length - 2] = byte(uint8(ctx.length >> 8));
        data[data.length - 1] = byte(uint8(ctx.length));
        // pack data with the replacement ctx
        return abi.encodePacked(
            data,
            ctx, new bytes(paddedLength - ctx.length) // ctx padding
        );
    }

}
