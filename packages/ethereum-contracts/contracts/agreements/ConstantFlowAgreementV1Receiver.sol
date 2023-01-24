// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { IConstantFlowAgreementV1Receiver } from "../interfaces/agreements/IConstantFlowAgreementV1Receiver.sol";

/**
 * Abstract base contract for a CFA receiver hook
 * Inheriting contracts are automatically subscribed to receive hooks
 * via ERC165 interface detection routed through ERC-1820.
 */
abstract contract ConstantFlowAgreementV1Receiver is IConstantFlowAgreementV1Receiver {
    // ERC165 Interface Detection
    function supportsInterface(bytes4 interfaceId) public view virtual returns (bool) {
        return
            interfaceId == 0x01ffc9a7 || // Interface ID for ERC165
            interfaceId == this.onFlowChanged.selector; // 0x882ed34b
    }
}
