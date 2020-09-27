// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

import "./ProxyUtils.sol";
import "./Proxy.sol";

/**
 * @dev Proxiable contract.
 *      Inspired by https://eips.ethereum.org/EIPS/eip-1822
 */
abstract contract Proxiable {

    /**
     * @dev Get current implementation code address.
     */
    function getCodeAddress() external view returns (address codeAddress){
        return ProxyUtils.implementation();
    }

    /**
     * @dev Proxiable UUID marker function.
     *      This would help to avoid wrong logic contract to be used for upgrading.
     */
    function proxiableUUID() public pure virtual returns (bytes32);

    /**
     * @dev Update code address function.
     *      It is internal, so the derived contract could setup its own permission logic.
     */
    function _updateCodeAddress(address newAddress) internal {
        require(
            proxiableUUID() == Proxiable(newAddress).proxiableUUID(),
            "Proxiable: NOT_COMPATIBLE"
        );
        ProxyUtils.setImplementation(newAddress);
    }

}
