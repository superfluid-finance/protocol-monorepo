// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity ^0.8.0;

import { FlowScheduler } from "./FlowScheduler.sol";
import {
    ISuperToken, IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

contract FlowSchedulerResolver {

    FlowScheduler public flowScheduler;

    constructor(address _flowScheduler) {
        flowScheduler = FlowScheduler(_flowScheduler);
    }

    /**
     * @dev Gelato resolver that checks whether Flow Scheduler action can be taken
     * @notice Make sure ACL permissions and ERC20 approvals are set for `flowScheduler` 
     *         before using Gelato automation with this resolver
     * @return bool whether there is a valid Flow Scheduler action to be taken or not
     * @return bytes the function payload to be executed (empty if none)
     */
     function checker(
        address superToken,
        address sender,
        address receiver
    ) external view returns( bool , bytes memory ) {

        FlowScheduler.FlowSchedule memory flowSchedule = flowScheduler.getFlowSchedule(
            superToken, 
            sender, 
            receiver
        );

        (, IConstantFlowAgreementV1 cfa) = flowScheduler.cfaV1();
        (,uint8 permissions, int96 flowRateAllowance) = cfa.getFlowOperatorData(
            ISuperToken(superToken), 
            sender, 
            address(flowScheduler)
        );
        (,int96 currentFlowRate,,) = cfa.getFlow(ISuperToken(superToken), sender, receiver);

        // 1. permissions must not be create/update/delete (7) or create/delete (5)
        // 2. scheduled flowRate must not be greater than allowance
        if ( ( permissions != 5 && permissions != 7 )        
             || flowSchedule.flowRate > flowRateAllowance ) {

            // return canExec as false and non-executable payload
            return (
                false,
                "0x"
            );

        }

        // 1. end date must be set (flow schedule exists)
        // 2. end date must have been past
        // 3. flow must have actually exist to be deleted
        else if ( flowSchedule.endDate != 0 &&
                  block.timestamp >= flowSchedule.endDate &&
                  currentFlowRate != 0) {

            // return canExec as true and executeDeleteFlow payload
            return (
                true,
                abi.encodeCall( FlowScheduler.executeDeleteFlow,
                    (
                        ISuperToken(superToken),
                        sender,
                        receiver,
                        "" // not supporting user data
                    )
                )
            );

        }
        
        // 1. start date must be set (flow schedule exists)
        // 2. start date must have been past
        // 3. max delay must have not been exceeded
        // 4. enough erc20 allowance to transfer the optional start amount
        else if ( flowSchedule.startDate != 0 &&
                  block.timestamp >= flowSchedule.startDate && 
                  block.timestamp <= flowSchedule.startDate + flowSchedule.startMaxDelay &&
                  ISuperToken(superToken).allowance(sender, address(flowScheduler)) >= flowSchedule.startAmount ) {
            
            // return canExec as true and executeCreateFlow payload
            return (
                true,
                abi.encodeCall( FlowScheduler.executeCreateFlow,
                    (
                        ISuperToken(superToken),
                        sender,
                        receiver,
                        "" // not supporting user data
                    )
                )
            );

        } else {

            // return canExec as false and non-executable payload
            return (
                false,
                "0x"
            );

        }

    }

}