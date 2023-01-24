
// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

/// @title interface for programmability of CFAv1 receivers
/// @notice With this interface, CFA receivers can set up hooks to be executed whenever
/// a flow they're receiving changes state.
/// In order to implement the hook in a contract acting as flow receiver (no delegation),
/// inherit from the abstract contract ConstantFlowAgreementV1ReceiveHook.
/// In order to delegate to an implementation of this interface from any contract or EOA, they
/// can set up the delegation with ERC1820.setInterfaceImplementer(), using
/// keccak("IConstantFlowAgreementV1ReceiveHook") as _interfaceHash.
interface IConstantFlowAgreementV1ReceiveHook {
    /// @dev if the hook reverts when setting the flowrate from non-zero to zero,
    /// the transaction will succeed anyway. In other cases, it will also revert.
    function onFlowChanged(
        address superToken,
        address operator,
        address sender,
        address receiver,
        int96 oldFlowRate,
        int96 newFlowRate
    ) external returns(bool);
}
