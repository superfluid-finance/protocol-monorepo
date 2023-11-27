import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "../../src/mappings/superTokenFactory";
import { assertEventBaseProperties, assertTokenStatisticProperties } from "../assertionHelpers";
import {
    daiAddress,
    daiName,
    daiSymbol,
    daiXAddress,
    daiXName,
    daiXSymbol,
    DEFAULT_DECIMALS,
    FAKE_SUPER_TOKEN_TOTAL_SUPPLY,
    FALSE,
    maticXAddress,
    maticXName,
    maticXSymbol,
    superTokenLogicAddress,
    TRUE,
} from "../constants";
import {
    createCustomSuperTokenCreatedEvent,
    createSuperTokenCreatedEvent,
    createSuperTokenLogicCreatedEvent,
} from "./superTokenFactory.helper";
import {
    mockedGetHost,
    mockedHandleSuperTokenInitRPCCalls,
} from "../mockedFunctions";
import { BIG_INT_ZERO, ZERO_ADDRESS } from "../../src/utils";
import { Address } from "@graphprotocol/graph-ts";

describe("SuperTokenFactory Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleSuperTokenCreated() - Should create a new SuperTokenCreatedEvent entity", () => {
            const SuperTokenCreatedEvent =
                createSuperTokenCreatedEvent(maticXAddress);

            mockedGetHost(maticXAddress);
            mockedHandleSuperTokenInitRPCCalls(
                maticXAddress,
                DEFAULT_DECIMALS,
                ZERO_ADDRESS,
                maticXName,
                maticXSymbol
            );

            handleSuperTokenCreated(SuperTokenCreatedEvent);

            const id = assertEventBaseProperties(
                SuperTokenCreatedEvent,
                "SuperTokenCreated"
            );
            assert.fieldEquals(
                "SuperTokenCreatedEvent",
                id,
                "token",
                maticXAddress
            );
        });

        test("handleCustomSuperTokenCreated() - Should create a new CustomSuperTokenCreatedEvent entity", () => {
            const CustomSuperTokenCreatedEvent =
                createCustomSuperTokenCreatedEvent(maticXAddress);

            mockedGetHost(maticXAddress);
            mockedHandleSuperTokenInitRPCCalls(
                maticXAddress,
                DEFAULT_DECIMALS,
                ZERO_ADDRESS,
                maticXName,
                maticXSymbol
            );

            handleCustomSuperTokenCreated(CustomSuperTokenCreatedEvent);

            const id = assertEventBaseProperties(
                CustomSuperTokenCreatedEvent,
                "CustomSuperTokenCreated"
            );
            assert.fieldEquals(
                "CustomSuperTokenCreatedEvent",
                id,
                "token",
                maticXAddress
            );
        });

        test("handleSuperTokenLogicCreated() - Should create a new SuperTokenLogicCreatedEvent entity", () => {
            const tokenLogic = superTokenLogicAddress;

            const SuperTokenLogicCreatedEvent =
                createSuperTokenLogicCreatedEvent(tokenLogic);

            mockedGetHost(superTokenLogicAddress);

            handleSuperTokenLogicCreated(SuperTokenLogicCreatedEvent);

            const id = assertEventBaseProperties(
                SuperTokenLogicCreatedEvent,
                "SuperTokenLogicCreated"
            );
            assert.fieldEquals(
                "SuperTokenLogicCreatedEvent",
                id,
                "tokenLogic",
                tokenLogic
            );
        });
    });

    describe("Higher Order Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        }); 

        test("handleSuperTokenCreated() - Should create a new Token entity (no underlying)", () => {
            const SuperTokenCreatedEvent =
                createSuperTokenCreatedEvent(maticXAddress);

            mockedHandleSuperTokenInitRPCCalls(
                maticXAddress,
                DEFAULT_DECIMALS,
                ZERO_ADDRESS,
                maticXName,
                maticXSymbol
            );

            handleSuperTokenCreated(SuperTokenCreatedEvent);

            assert.fieldEquals("Token", maticXAddress, "id", maticXAddress);
            assert.fieldEquals("Token", maticXAddress, "createdAtTimestamp", SuperTokenCreatedEvent.block.timestamp.toString());
            assert.fieldEquals("Token", maticXAddress, "createdAtBlockNumber", SuperTokenCreatedEvent.block.number.toString());
            assert.fieldEquals("Token", maticXAddress, "decimals", DEFAULT_DECIMALS.toString());
            assert.fieldEquals("Token", maticXAddress, "name", maticXName);
            assert.fieldEquals("Token", maticXAddress, "symbol", maticXSymbol);
            assert.fieldEquals("Token", maticXAddress, "isSuperToken", TRUE);
            assert.fieldEquals("Token", maticXAddress, "isNativeAssetSuperToken", TRUE);
            assert.fieldEquals("Token", maticXAddress, "isListed", FALSE);
            assert.fieldEquals("Token", maticXAddress, "underlyingAddress", ZERO_ADDRESS.toHexString());
            assert.fieldEquals("Token", maticXAddress, "underlyingToken", ZERO_ADDRESS.toHexString());
        });

        test("handleSuperTokenCreated() - Should create a new Token entity (with underlying)", () => {
            const SuperTokenCreatedEvent =
                createSuperTokenCreatedEvent(daiXAddress);

            mockedGetHost(daiXAddress);
            // for getOrInitSuperToken (DAIx)
            mockedHandleSuperTokenInitRPCCalls(
                daiXAddress,
                DEFAULT_DECIMALS,
                Address.fromString(daiAddress),
                daiXName,
                daiXSymbol
            );
            // for getOrInitToken ((PoS) Dai Stablecoin (DAI))
            mockedHandleSuperTokenInitRPCCalls(
                daiAddress,
                DEFAULT_DECIMALS,
                ZERO_ADDRESS,
                daiName,
                daiSymbol
            );

            handleSuperTokenCreated(SuperTokenCreatedEvent);
            
            // Validate Created SuperToken properties
            assert.fieldEquals("Token", daiXAddress, "id", daiXAddress);
            assert.fieldEquals("Token", daiXAddress, "createdAtTimestamp", SuperTokenCreatedEvent.block.timestamp.toString());
            assert.fieldEquals("Token", daiXAddress, "createdAtBlockNumber", SuperTokenCreatedEvent.block.number.toString());
            assert.fieldEquals("Token", daiXAddress, "decimals", DEFAULT_DECIMALS.toString());
            assert.fieldEquals("Token", daiXAddress, "name", daiXName);
            assert.fieldEquals("Token", daiXAddress, "symbol", daiXSymbol);
            assert.fieldEquals("Token", daiXAddress, "isSuperToken", TRUE);
            assert.fieldEquals("Token", daiXAddress, "isNativeAssetSuperToken", FALSE);
            assert.fieldEquals("Token", daiXAddress, "isListed", FALSE);
            assert.fieldEquals("Token", daiXAddress, "underlyingAddress", daiAddress);
            assert.fieldEquals("Token", daiXAddress, "underlyingToken", daiAddress);

            // Validate Created Underlying Token properties
            assert.fieldEquals("Token", daiAddress, "id", daiAddress);
            assert.fieldEquals("Token", daiAddress, "createdAtTimestamp", SuperTokenCreatedEvent.block.timestamp.toString());
            assert.fieldEquals("Token", daiAddress, "createdAtBlockNumber", SuperTokenCreatedEvent.block.number.toString());
            assert.fieldEquals("Token", daiAddress, "decimals", DEFAULT_DECIMALS.toString());
            assert.fieldEquals("Token", daiAddress, "name", daiName);
            assert.fieldEquals("Token", daiAddress, "symbol", daiSymbol);
            assert.fieldEquals("Token", daiAddress, "isSuperToken", FALSE);
            assert.fieldEquals("Token", daiAddress, "isNativeAssetSuperToken", FALSE);
            assert.fieldEquals("Token", daiAddress, "isListed", FALSE);
            assert.fieldEquals("Token", daiAddress, "underlyingAddress", ZERO_ADDRESS.toHexString());

            // NOTE: underlyingToken property is undefined on Token schema for non super tokens
            // assert.fieldEquals("Token", daiAddress, "underlyingToken", undefined);
        });

        // NOTE: There is no need to duplicate these tests for handleCustomSuperTokenCreated because we are
        // really testing out getOrInitSuperToken at the core.
    });

    describe("Aggregate Entity Mapping Tests", () => {
        test("handleSuperTokenCreated() - should create TokenStatistic and TokenStatisticLog entity", () => {
            const SuperTokenCreatedEvent =
                createSuperTokenCreatedEvent(maticXAddress);

            mockedHandleSuperTokenInitRPCCalls(
                maticXAddress,
                DEFAULT_DECIMALS,
                ZERO_ADDRESS,
                maticXName,
                maticXSymbol
            );

            handleSuperTokenCreated(SuperTokenCreatedEvent);

            // Validate Created TokenStatistic properties
            assertTokenStatisticProperties(
                SuperTokenCreatedEvent,
                "SuperTokenCreated",
                maticXAddress,
                SuperTokenCreatedEvent.block.timestamp,
                SuperTokenCreatedEvent.block.number,
                0,                             // totalNumberOfActiveStreams
                0,                             // totalNumberOfClosedStreams
                0,                             // totalNumberOfIndexes
                0,                             // totalNumberOfActiveIndexes
                0,                             // totalSubscriptionsWithUnits
                0,                             // totalApprovedSubscriptions
                BIG_INT_ZERO,                  // totalDeposit
                BIG_INT_ZERO,                  // totalOutflowRate
                BIG_INT_ZERO,                  // totalAmountStreamedUntilUpdatedAt
                BIG_INT_ZERO,                  // totalAmountTransferredUntilUpdatedAt
                BIG_INT_ZERO,                  // totalAmountDistributedUntilUpdatedAt
                FAKE_SUPER_TOKEN_TOTAL_SUPPLY,  // totalSupply = 100,
                0, // totalNumberOfAccounts
                0 // totalNumberOfHolders
            );

        });
    });
});
