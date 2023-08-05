// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { ISuperfluid, ISuperAgreement, ISuperToken } from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import { ConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";

/**
 * @title Batch liquidator contract
 * @author Superfluid
 * @dev This contract allows to delete multiple flows in a single transaction.
 * @notice Reduces calldata by having host and cfa hardcoded, this can make a significant difference in tx fees on L2s
 */

contract BatchLiquidator {

    error ARRAY_SIZES_DIFFERENT();

    address public immutable host;
    address public immutable cfa;

    constructor(address host_, address cfa_) {
        host = host_;
        cfa = cfa_;
    }

    /**
     * @dev Delete flows in batch
     * @param superToken - The super token the flows belong to.
     * @param senders - List of senders.
     * @param receivers - Corresponding list of receivers.
     */
    function deleteFlows(
        address superToken,
        address[] calldata senders, address[] calldata receivers
    ) external {
        uint256 length = senders.length;
        if(length != receivers.length) revert ARRAY_SIZES_DIFFERENT();
        for (uint256 i; i < length;) {
            // We tolerate any errors occured during liquidations.
            // It could be due to flow had been liquidated by others.
            // solhint-disable-next-line avoid-low-level-calls
            address(host).call(
                abi.encodeCall(
                    ISuperfluid(host).callAgreement,
                    (
                        ISuperAgreement(cfa),
                        abi.encodeCall(
                            IConstantFlowAgreementV1(cfa).deleteFlow,
                            (
                                ISuperToken(superToken),
                                senders[i],
                                receivers[i],
                                new bytes(0)
                            )
                        ),
                        new bytes(0)
                    )
                )
            );
            unchecked { i++; }
        }

        // If the liquidation(s) resulted in any super token
        // rewards, send them all to the sender instead of having them
        // locked in the contract
        {
            uint256 balance = ERC20(superToken).balanceOf(address(this));
            if (balance > 0) {
                ERC20(superToken).transferFrom(address(this), msg.sender, balance);
            }
        }
    }

    // single flow delete with check for success
    function deleteFlow(address superToken, address sender, address receiver) external {
        /* solhint-disable */
        (bool success, bytes memory returndata) = address(host).call(
            abi.encodeCall(
                ISuperfluid(host).callAgreement,
                (
                    ISuperAgreement(cfa),
                    abi.encodeCall(
                        IConstantFlowAgreementV1(cfa).deleteFlow,
                        (
                            ISuperToken(superToken),
                            sender,
                            receiver,
                            new bytes(0)
                        )
                    ),
                    new bytes(0)
                )
            )
        );
        if (!success) {
            if (returndata.length == 0) revert();
            // solhint-disable
            assembly {
                revert(add(32, returndata), mload(returndata))
            }
        }
        /* solhint-enable */
        // If the liquidation(s) resulted in any super token
        // rewards, send them all to the sender instead of having them
        // locked in the contract
        {
            uint256 balance = ERC20(superToken).balanceOf(address(this));
            if (balance > 0) {
                ERC20(superToken).transferFrom(address(this), msg.sender, balance);
            }
        }
    }

    // Deletes outflows of the given sender account as long as there is enough gas
    // Requires a FlowNFT contract keeping track of the outflows
    function deleteFlowsUsingFlowNFT(address superToken, address sender, ConstantOutflowNFT flowNFT) external {
        // this is not a scientific limit. Idea: don't run out of gas for typical (non-SuperApp) liquidations
        while (gasleft() > 500_000) {
            address receiver = flowNFT.nextFlowReceiver(superToken);
            if (receiver == address(0)) break;
            this.deleteFlow(superToken, sender, receiver);
        }
    }
}
