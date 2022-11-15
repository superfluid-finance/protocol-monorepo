import { expect } from "chai";
import { fetchEntityAndEnsureExistence } from "../../helpers/helpers";
import {
    IAccountTokenSnapshot,
    IEvent,
    IExpectedFlowOperatorData,
    IFlowOperator,
} from "../../interfaces";
import { getAccountTokenSnapshot } from "../../queries/aggregateQueries";
import { getFlowOperator } from "../../queries/holQueries";
import { validateReverseLookup } from "../validators";

export const fetchFlowOperatorAndValidate = async ({
    event,
    expectedFlowOperator,
    isCreate,
}: {
    event: IEvent;
    expectedFlowOperator: IExpectedFlowOperatorData;
    isCreate: boolean;
}) => {
    const flowOperatorId = expectedFlowOperator.id;
    const flowOperatorEntity =
        await fetchEntityAndEnsureExistence<IFlowOperator>(
            getFlowOperator,
            flowOperatorId,
            "FlowOperator"
        );

    validateFlowOperator(flowOperatorEntity, expectedFlowOperator);

    // validate flow operator updated events reverse lookup
    validateReverseLookup(event, flowOperatorEntity.flowOperatorUpdatedEvents);

    if (isCreate) {
        await validateATSReverseLookupsForFlowOperator(flowOperatorEntity);
    }
};

const validateATSReverseLookupsForFlowOperator = async (
    flowOperator: IFlowOperator
) => {
    const senderATS =
        await fetchEntityAndEnsureExistence<IAccountTokenSnapshot>(
            getAccountTokenSnapshot,
            flowOperator.sender.id + "-" + flowOperator.token.id,
            "AccountTokenSnapshot"
        );
    const flowOperatorLightEntity = { id: flowOperator.id };
    validateReverseLookup(flowOperatorLightEntity, senderATS.flowOperators);
};

const validateFlowOperator = (
    flowOperatorEntity: IFlowOperator,
    expectedFlowOperator: IExpectedFlowOperatorData
) => {
    expect(flowOperatorEntity.id, "Flow Operator: id error").to.equal(
        expectedFlowOperator.id
    );
    expect(
        flowOperatorEntity.permissions,
        "Flow Operator: permissions error"
    ).to.equal(expectedFlowOperator.permissions);
    expect(
        flowOperatorEntity.flowRateAllowanceGranted,
        "Flow Operator: flowRateAllowanceGranted error"
    ).to.equal(expectedFlowOperator.flowRateAllowanceGranted);
    expect(
        flowOperatorEntity.flowRateAllowanceRemaining,
        "Flow Operator: flowRateAllowanceRemaining error"
    ).to.equal(expectedFlowOperator.flowRateAllowanceRemaining);
};
