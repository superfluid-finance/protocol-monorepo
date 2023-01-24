// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { IConstantFlowAgreementV1ReceiveHook } from "../interfaces/agreements/IConstantFlowAgreementV1ReceiveHook.sol";

/**
 * Abstract base contract for a CFA receiver hook
 * Inheriting contracts are automatically subscribed to receive hooks
 * via ERC165 interface detection routed through ERC-1820.
 */
abstract contract ConstantFlowAgreementV1ReceiveHook is IConstantFlowAgreementV1ReceiveHook {
    // ERC165 Interface Detection
    function supportsInterface(bytes4 interfaceId) public view virtual returns (bool) {
        return
            interfaceId == 0x01ffc9a7 || // Interface ID for ERC165
            interfaceId == this.onFlowChanged.selector; // 0x882ed34b
    }
}
