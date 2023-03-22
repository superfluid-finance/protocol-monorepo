// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperToken } from "./ISuperToken.sol";
import { IFlowNFTBase } from "./IFlowNFTBase.sol";

interface IConstantOutflowNFT is IFlowNFTBase {
    /**************************************************************************
     * Write Functions
     *************************************************************************/

    /// @notice The onCreate function is called when a new flow is created.
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onCreate(address flowSender, address flowReceiver) external;

    /// @notice The onUpdate function is called when a flow is updated.
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onUpdate(address flowSender, address flowReceiver) external;

    /// @notice The onDelete function is called when a flow is deleted.
    /// @param flowSender the flow sender
    /// @param flowReceiver the flow receiver
    function onDelete(address flowSender, address flowReceiver) external;
}
