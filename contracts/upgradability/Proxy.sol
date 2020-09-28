// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

import "./ProxyUtils.sol";

/**
 * @title Proxy
 * @dev Implements delegation of calls to other contracts, with proper
 * forwarding of return values and bubbling of failures.
 * It defines a fallback function that delegates all calls to the implementation.
 */
contract Proxy {

    // Empty constructor is more friendlier to CREATE2, otherwise parameters will be part of the
    // address computation. Use initalizeProxy for initializing the logic contract address.

    /**
     * @dev Proxy initialization function.
     *      This should only be called once and it is permission-less.
     * @param initialAddress Initial logic contract code address to be used.
     */
    function initializeProxy(address initialAddress) external {
        require(initialAddress != address(0), "Proxy: INITIALIZED_WITH_ZERO_ADDRESS");
        require(ProxyUtils.implementation() == address(0), "Proxy: ALREADY_INITIALIZED");
        ProxyUtils.setImplementation(initialAddress);
    }

    /**
     * @dev Fallback function.
     * Implemented entirely in `_delegate`.
     */
    fallback () external payable {
        _delegate(ProxyUtils.implementation());
    }

    /**
     * @dev Receive function.
     * It should be forbidden.
     */
    receive () external payable {
        require(false, "Proxy: RECEIVE_FORBIDDEN");
    }

    /**
     * @dev Delegates execution to an implementation contract.
     *
     * Original implementation:
     * https://github.com/OpenZeppelin/openzeppelin-sdk/blob/master/packages/lib/contracts/upgradeability/Proxy.sol
     */
    function _delegate(address implementation) internal {
        assembly { // solium-disable-line
            // Copy msg.data. We take full control of memory in this inline assembly
            // block because it will not return to Solidity code. We overwrite the
            // Solidity scratch pad at memory position 0.
            calldatacopy(0, 0, calldatasize())

            // Call the implementation.
            // out and outsize are 0 because we don't know the size yet.
            let result := delegatecall(gas(), implementation, 0, calldatasize(), 0, 0)

            // Copy the returned data.
            returndatacopy(0, 0, returndatasize())

            switch result
            // delegatecall returns 0 on error.
            case 0 { revert(0, returndatasize()) }
            default { return(0, returndatasize()) }
        }
    }

}
