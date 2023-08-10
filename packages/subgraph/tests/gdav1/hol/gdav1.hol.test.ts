// import { Address, BigInt } from "@graphprotocol/graph-ts";
// import { assert, beforeEach, clearStore, describe, test } from "matchstick-as/assembly/index";
// import { handleFlowOperatorUpdated } from "../../../src/mappings/cfav1";
// import {
//     BIG_INT_ZERO,
//     getAccountTokenSnapshotID,
//     getFlowOperatorID,
//     getStreamID,
//     ZERO_ADDRESS,
// } from "../../../src/utils";
// import { assertHigherOrderBaseProperties } from "../../assertionHelpers";
// import { alice, bob, maticXAddress, maticXName, maticXSymbol, superfluidPool } from "../../constants";
// import {
//     createBufferAdjustedEvent,
//     createDistributionClaimedEvent,
//     createFlowDistributionUpdatedEvent,
//     createInstantDistributionUpdatedEvent,
//     createMemberUnitsUpdatedEvent,
//     createPoolConnectionUpdatedEvent,
//     createPoolCreatedEvent,
// } from "../gdav1.helper";
// import { mockedApprove } from "../../mockedFunctions";
// import { Pool } from "../../../generated/schema";

// const initialFlowRate = BigInt.fromI32(100);
// const superToken = maticXAddress;

// describe("GeneralDistributionAgreementV1 Higher Order Level Entity Unit Tests", () => {
//     beforeEach(() => {
//         clearStore();
//     });

//     test("handlePoolCreated() - Should create a new Pool entity (create)", () => {
//         const admin = alice;
//         // create pool
//         const poolCreatedEvent = createPoolCreatedEvent(superToken, admin, superfluidPool);

//         const id = poolCreatedEvent.params.pool.toHexString();
//         const deposit = getDeposit(flowUpdatedEvent.params.flowRate);
//         const streamedUntilUpdatedAt = _getStreamedUntilUpdatedAt(
//             BIG_INT_ZERO,
//             flowUpdatedEvent.block.timestamp,
//             BIG_INT_ZERO,
//             BIG_INT_ZERO
//         );
//         assertHigherOrderBaseProperties("Pool", id, poolCreatedEvent);
//         assert.fieldEquals("Pool", id, "currentFlowRate", poolCreatedEvent.params.flowRate.toString());
//         assert.fieldEquals("Pool", id, "deposit", deposit.toString());
//         assert.fieldEquals("Pool", id, "token", poolCreatedEvent.params.token.toHexString());
//         assert.fieldEquals("Pool", id, "sender", poolCreatedEvent.params.sender.toHexString());
//         assert.fieldEquals("Pool", id, "receiver", poolCreatedEvent.params.receiver.toHexString());
//         assert.fieldEquals("Pool", id, "userData", poolCreatedEvent.params.userData.toHexString());
//     });

//     // Pool
//         // handlePoolConnectionUpdated
//         // handleBufferAdjusted
//         // handleFlowDistributionUpdated
//         // handleInstantDistributionUpdated
//         // handleDistributionClaimed
//         // handleMemberUnitsUpdated
//             // connected member
//             // disconnected member
//             // 0 => some units
//                 // connected member
//                 // disconnected member
//             // some units => 0
//                 // connected member
//                 // disconnected member

//     // PoolMember
//         // handlePoolConnectionUpdated
//         // handleDistributionClaimed
//         // handleMemberUnitsUpdated
//     // PoolDistributor
//         // handleBufferAdjusted
//         // handleFlowDistributionUpdated
//         // handleInstantDistributionUpdated
// });
