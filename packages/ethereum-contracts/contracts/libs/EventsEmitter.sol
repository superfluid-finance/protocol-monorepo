// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

/**
 * @title Events Emitter Library
 * @author Superfluid
 * @dev A library used for emitting missing and unaccessable events.
 *
 */
library EventsEmitter {
    event Transfer(address indexed from, address indexed to, uint256 value);

    function emitTransfer(address from, address to, uint256 value) internal {
        emit Transfer(from, to, value);
    }
}