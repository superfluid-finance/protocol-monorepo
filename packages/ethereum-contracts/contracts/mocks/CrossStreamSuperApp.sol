// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/console.sol";

import {
    IConstantFlowAgreementV1,
    ISuperfluid,
    ISuperfluidToken,
    ISuperToken
} from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperAppBaseFlow } from "../apps/SuperAppBaseFlow.sol";
import { SuperTokenV1Library } from "../apps/SuperTokenV1Library.sol";

using SuperTokenV1Library for ISuperToken;

contract CrossStreamSuperApp is SuperAppBaseFlow {
    address public flowRecipient;
    address public prevSender;
    int96 public prevFlowRate;

    constructor(ISuperfluid host_, address z_) SuperAppBaseFlow(host_, true, true, true, "") {
        flowRecipient = z_;
    }

    function onFlowCreated(ISuperToken superToken, address sender, bytes calldata ctx)
        internal
        override
        returns (bytes memory newCtx)
    {
        newCtx = ctx;

        int96 inFlowRate = superToken.getFlowRate(sender, address(this));

        // get incoming stream
        if (prevSender == address(0)) {
            // console.log("\n====== Getting First Stream");

            // printAppInfo("=== tx1 initial state", newCtx, superToken);

            // create outflow (1 to 1 forwarding)
            newCtx = superToken.createFlowWithCtx(flowRecipient, inFlowRate, newCtx);
            // printAppInfo("====== after creating outflow", newCtx, superToken);
        } else {
            // console.log("\n====== Getting Second Stream");

            // printAppInfo("=== tx2 initial state", newCtx, superToken);

            // update outgoing flow to new incoming flowrate (keep 1 to 1 forwarding)
            newCtx = superToken.updateFlowWithCtx(flowRecipient, inFlowRate, newCtx);
            // printAppInfo("=== after updating outflow", newCtx, superToken);

            // //alternative to update: delete and re-create outgoing flow
            // newCtx = superToken.deleteFlowWithCtx(address(this), flowRecipient, newCtx);
            // printAppInfo("=== after deleting outflow", newCtx, superToken);
            // newCtx = superToken.createFlowWithCtx(flowRecipient, inFlowRate, newCtx);
            // printAppInfo("=== after re-creating outflow", newCtx, superToken);

            // delete previous incoming flow. Why does this revert?
            newCtx = superToken.deleteFlowWithCtx(prevSender, address(this), newCtx);
            // printAppInfo("=== after deleting first inflow", newCtx, superToken);
        }

        prevSender = sender;
        prevFlowRate = inFlowRate;
    }

    function printAppInfo(string memory marker, bytes memory ctx, ISuperToken superToken) internal view {
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(
            address(HOST.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")))
        );

        ISuperfluid.Context memory sfContext = HOST.decodeCtx(ctx);
        console.log("");
        console.log(marker);
        console.log("ctx app credit granted", sfContext.appCreditGranted);
        console.log("ctx app credit used:");
        console.logInt(sfContext.appCreditUsed);

        int256 remainingAppCredit = int256(sfContext.appCreditGranted) - sfContext.appCreditUsed;
        console.log("ctx app credit remaining:");
        console.logInt(int256(sfContext.appCreditGranted) - sfContext.appCreditUsed);
        console.log("app netflowrate", uint256(uint96(superToken.getNetFlowRate(address(this)))));
        console.log("app balance: ", superToken.balanceOf(address(this)));

        (int256 availableBalance, uint256 deposit, uint256 owedDeposit,) =
            ISuperfluidToken(address(superToken)).realtimeBalanceOfNow(address(this));
        console.log("app availableBalance:");
        console.logInt(availableBalance);
        console.log("app deposit", deposit);
        console.log("app owedDeposit", owedDeposit);

        int96 maxRemainingFr = cfa.getMaximumFlowRateFromDeposit(superToken, uint256(remainingAppCredit));
        console.log("remaining max flowrate from deposit:");
        console.logInt(maxRemainingFr);
    }
}
