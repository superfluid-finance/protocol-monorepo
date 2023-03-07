// SPDX-License-Identifier: UNLICENSED
pragma solidity >= 0.8.4;

import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";

/**
 * @dev The interface for any super token pool regardless of the distribution schemes.
 */
interface ISuperTokenPool {
    function getPendingDistribution() external view returns (Value);

    function distributionAllowedFrom(address account) external view returns (bool);

    function getIndex() external view returns (PDPoolIndex memory);

    function getClaimable(Time t, address memberAddr) external view returns (Value);

    function claimAll(Time t, address memberAddr) external returns (bool);

    function operatorSetIndex(PDPoolIndex calldata index) external returns (bool);

   // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    function operatorConnectMember(Time t, address memberAddr, bool doConnect) external returns (bool);
}
