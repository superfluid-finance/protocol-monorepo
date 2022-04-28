// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { ISuperfluid, ISuperAgreement } from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

/**
 * @title Batch liquidator contract
 * @author Superfluid
 */
contract BatchLiquidator {

    /**
     * @dev Delete flows in batch
     * @param host - The host contract address.
     * @param cfa  - The cfa contract address.
     * @param superToken - The super token the flows belong to.
     * @param senders - List of senders.
     * @param receivers - Corresponding list of receivers.
     */
    function deleteFlows(
        address host,
        address cfa,
        address superToken,
        address[] calldata senders, address[] calldata receivers
    ) external {
        require(senders.length == receivers.length, "arrays different sizes");

        for (uint i = 0; i < senders.length; ++i) {
            bool success;
            // We tolerate any errors occured during liquidations.
            // It could be due to flow had been liquidated by others.
            // solhint-disable-next-line avoid-low-level-calls
            (success, ) = address(host).call(
                abi.encodeWithSelector(
                    ISuperfluid.callAgreement.selector,
                    cfa,
                    abi.encodeWithSelector(
                        IConstantFlowAgreementV1.deleteFlow.selector,
                        superToken,
                        senders[i],
                        receivers[i],
                        new bytes(0)
                    ),
                    new bytes(0)
                )
            );
        }

        // If the liquidation(s) resulted in any super token
        // rewards, send them all to the sender instead of having them
        // locked in the contract
        {
            uint256 balance = ERC20(superToken).balanceOf(address(this));
            if(balance > 0) {
                ERC20(superToken).transferFrom(address(this), msg.sender, balance);
            }
        }
    }
}

