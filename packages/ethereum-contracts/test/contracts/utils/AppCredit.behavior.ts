// -- Monetary Unit Data
interface MonetaryUnitData {
    // TODO: add availableBalance
    readonly deposit: number;
    readonly owedDeposit: number;
}

// -- Agreement Data
interface AgreementData {
    readonly buffer: number;
    readonly appCredit: number;
}

// -- App induced deposit delta
// --   Notes on EVMv1 Impl:
// --     * This is tracked using ctx
// --     * Each time App callAgreementWithCtx, it uses ctxUseAppCredits to update this
// INPUT
interface AgreementOperation {
    readonly newBuffer: number;
    readonly currentAgreementData: AgreementData;
    readonly currentSenderMUD: MonetaryUnitData;
    readonly currentReceiverMUD: MonetaryUnitData;
    readonly isReceiverApp?: boolean;
}

// OUTPUT
interface AgreementDataStates {
    readonly initialAgreementData: AgreementData;
    readonly modifiedAgreementData: AgreementData;
}

interface MonetaryUnitDataStates {
    readonly initialMonetaryUnitData: MonetaryUnitData;
    readonly modifiedMonetaryUnitData: MonetaryUnitData;
}

interface AgreementOperationOutput {
    readonly agreementDataStates: AgreementDataStates;
    readonly senderMUDStates: MonetaryUnitDataStates;
    readonly receiverMUDStates: MonetaryUnitDataStates;
    readonly callbackGeneratedDataList: AgreementOperationOutput[];
}

interface AgreementOperationOutputDelta {
    readonly receiverDepositDelta: number;
    readonly receiverOwedDepositDelta: number;
    readonly senderDepositDelta: number;
    readonly senderOwedDepositDelta: number;
    readonly agreementBufferDelta: number;
    readonly agreementAppCreditDelta: number;
}

// Helper Functions
const getMUDDepositDelta = (mudStates: MonetaryUnitDataStates) => {
    return (
        mudStates.modifiedMonetaryUnitData.deposit -
        mudStates.initialMonetaryUnitData.deposit
    );
};
const getMUDOwedDepositDelta = (mudStates: MonetaryUnitDataStates) => {
    return (
        mudStates.modifiedMonetaryUnitData.owedDeposit -
        mudStates.initialMonetaryUnitData.owedDeposit
    );
};

const getAgreementBufferDelta = (agreementStates: AgreementDataStates) => {
    return (
        agreementStates.modifiedAgreementData.buffer -
        agreementStates.initialAgreementData.buffer
    );
};
const getAgreementAppCreditDelta = (agreementStates: AgreementDataStates) => {
    return (
        agreementStates.modifiedAgreementData.appCredit -
        agreementStates.initialAgreementData.appCredit
    );
};

const getDeltas = (
    agreementOperationOutputs: AgreementOperationOutput[]
): AgreementOperationOutputDelta[] => {
    return agreementOperationOutputs.map((x) => {
        const agreement = x.agreementDataStates;
        const sender = x.senderMUDStates;
        const receiver = x.receiverMUDStates;
        return {
            receiverDepositDelta: getMUDDepositDelta(receiver),
            receiverOwedDepositDelta: getMUDOwedDepositDelta(receiver),
            senderDepositDelta: getMUDDepositDelta(sender),
            senderOwedDepositDelta: getMUDOwedDepositDelta(sender),
            agreementBufferDelta: getAgreementBufferDelta(agreement),
            agreementAppCreditDelta: getAgreementAppCreditDelta(agreement),
        };
    });
};

const ADDITIONAL_AMOUNT = 5;

const applyAdditionalAppCreditRule = (newBuffer: number, bufferDelta: number) => {
    if (newBuffer === 0) {
        return bufferDelta;
    }
    return bufferDelta + ADDITIONAL_AMOUNT;
}

const getSummedAgreementBufferDelta = (
    deltas: AgreementOperationOutputDelta[]
) => {
    return deltas.map((x) => x.agreementBufferDelta).reduce((a, b) => a + b, 0);
};

const initialBufferFunction = (
    agreementOperation: AgreementOperation,
    callbackAgreementOperations: AgreementOperation[]
): AgreementOperationOutput => {
    // recursively call this bufferFunction on any callbackAgreementOperations to get an array of
    // callbackAgreementOperationOutputs
    const callbackAgreementOperationOutputs = callbackAgreementOperations.map(
        (x) => initialBufferFunction(x, [])
    );

    // get array of deltas of all the relevant fields (sender/receiver mud, agreementData)
    const deltas = getDeltas(callbackAgreementOperationOutputs);

    // get the sum of callback agreement buffer deltas (how much the app will need to borrow/return from the user)
    const callbackBufferDeltaSum = getSummedAgreementBufferDelta(deltas);
    const currentAgreement = agreementOperation.currentAgreementData;

    // buffer granted in modified agreement vs previous buffer delta
    const bufferDelta = applyAdditionalAppCreditRule(
        agreementOperation.newBuffer,
        agreementOperation.newBuffer - currentAgreement.buffer
    );

    // app credit rule formula - amount app credit should change by given modified agreements in callback
    const appCreditDelta = Math.max(
        Math.min(bufferDelta, callbackBufferDeltaSum),
        -agreementOperation.currentAgreementData.appCredit
    );

    return {
        agreementDataStates: {
            initialAgreementData: currentAgreement,
            modifiedAgreementData: {
                // set new buffer for modified agreement
                buffer: agreementOperation.newBuffer,

                // add appCreditDelta (as calculated above) to current app credit
                appCredit: currentAgreement.appCredit + appCreditDelta,
            },
        },
        senderMUDStates: {
            initialMonetaryUnitData: agreementOperation.currentSenderMUD,
            modifiedMonetaryUnitData: {
                // add bufferDelta and appCreditDelta to the current sender deposit
                // it is the sender's responsibility to
                deposit:
                    agreementOperation.currentSenderMUD.deposit +
                    bufferDelta +
                    appCreditDelta,

                // the agreement sender does not ever accrue owedDeposit
                owedDeposit: agreementOperation.currentSenderMUD.owedDeposit,
            },
        },
        receiverMUDStates: {
            initialMonetaryUnitData: agreementOperation.currentReceiverMUD,
            modifiedMonetaryUnitData: {
                deposit:
                    agreementOperation.currentReceiverMUD.deposit +
                    callbackBufferDeltaSum,
                owedDeposit:
                    agreementOperation.currentReceiverMUD.owedDeposit +
                    appCreditDelta,
            },
        },
        callbackGeneratedDataList: callbackAgreementOperationOutputs,
    };
};

const nextBufferFunction = (
    output: AgreementOperationOutput,
    newEntryBuffer: number,
    newCallbackBuffer?: number
) => {
    const newOperation: AgreementOperation = {
        newBuffer: newEntryBuffer,
        currentAgreementData: output.agreementDataStates.modifiedAgreementData,
        currentSenderMUD: output.senderMUDStates.modifiedMonetaryUnitData,
        currentReceiverMUD: output.receiverMUDStates.modifiedMonetaryUnitData,
    };
    const newCallbackOperations: AgreementOperation[] =
        output.callbackGeneratedDataList.map((x) => ({
            newBuffer: newCallbackBuffer || newEntryBuffer,
            currentAgreementData: x.agreementDataStates.modifiedAgreementData,
            currentSenderMUD: x.senderMUDStates.modifiedMonetaryUnitData,
            currentReceiverMUD: x.receiverMUDStates.modifiedMonetaryUnitData,
        }));

    return initialBufferFunction(newOperation, newCallbackOperations);
};

const printOutput = (title: string, output: AgreementOperationOutput) => {
    console.log(title);
    console.log(JSON.stringify(output, null, 4));
};
const INIT_MUD: MonetaryUnitData = {
    deposit: 0,
    owedDeposit: 0,
};
const INIT_AGREEMENT_DATA: AgreementData = {
    appCredit: 0,
    buffer: 0,
};
// CASE 1 - no callback
const c1_agreementOperation0: AgreementOperation = {
    newBuffer: 1,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c1a_result = initialBufferFunction(c1_agreementOperation0, []);
printOutput("CASE 1a: NO CB CREATE", c1a_result);
const c1b_result = nextBufferFunction(c1a_result, 2);
printOutput("CASE 1b: NO CB UPDATE (INCREMENT)", c1b_result);
const c1c_result = nextBufferFunction(c1b_result, 1);
printOutput("CASE 1c: NO CB UPDATE (DECREMENT)", c1c_result);
const c1d_result = nextBufferFunction(c1c_result, 1);
printOutput("CASE 1d: NO CB UPDATE (SAME)", c1d_result);
const c1e_result = nextBufferFunction(c1d_result, 0);
printOutput("CASE 1e: NO CB DELETE", c1e_result);

// CASE 2 - 1-to-1 forwarding @ 100% INPUT
const c2_agreementOperation0: AgreementOperation = {
    newBuffer: 1,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c2_callback_agreementOperation0: AgreementOperation = {
    newBuffer: 1,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c2a_result = initialBufferFunction(c2_agreementOperation0, [
    c2_callback_agreementOperation0,
]);
printOutput("CASE 2a: 1-TO-1 FORWARDING CREATE", c2a_result);
const c2b_result = nextBufferFunction(c2a_result, 2);
printOutput("CASE 2b: 1-TO-1 FORWARDING UPDATE (INCREMENT)", c2b_result);
const c2c_result = nextBufferFunction(c2b_result, 1);
printOutput("CASE 2c: 1-TO-1 FORWARDING UPDATE (DECREMENT)", c2c_result);
const c2d_result = nextBufferFunction(c2c_result, 1);
printOutput("CASE 2d: 1-TO-1 FORWARDING UPDATE (SAME)", c2d_result);
const c2e = nextBufferFunction(c2d_result, 0);
printOutput("CASE 2e: 1-TO-1 FORWARDING DELETE", c2e);

// CASE 3 - 1-to-1 forwarding @ 50% INPUT
const c3_agreementOperation0: AgreementOperation = {
    newBuffer: 1,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c3_callback_agreementOperation0: AgreementOperation = {
    newBuffer: 0.5,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c3a_result = initialBufferFunction(c3_agreementOperation0, [
    c3_callback_agreementOperation0,
]);
printOutput("CASE 3a: 1-TO-1 FORWARDING CREATE", c3a_result);
const c3b_result = nextBufferFunction(c3a_result, 2, 1);
printOutput("CASE 3b: 1-TO-1 FORWARDING UPDATE (INCREMENT)", c3b_result);
const c3c_result = nextBufferFunction(c3b_result, 1, 0.5);
printOutput("CASE 3c: 1-TO-1 FORWARDING UPDATE (DECREMENT)", c3c_result);
const c3d_result = nextBufferFunction(c3c_result, 1, 0.5);
printOutput("CASE 3d: 1-TO-1 FORWARDING UPDATE (SAME)", c3d_result);
const c3e = nextBufferFunction(c3d_result, 0);
printOutput("CASE 3e: 1-TO-1 FORWARDING DELETE", c3e);

// CASE 4 - 1-to-1 forwarding @ 150% INPUT
const c4_agreementOperation0: AgreementOperation = {
    newBuffer: 1,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c4_callback_agreementOperation0: AgreementOperation = {
    newBuffer: 1.5,
    currentAgreementData: INIT_AGREEMENT_DATA,
    currentSenderMUD: INIT_MUD,
    currentReceiverMUD: INIT_MUD,
};
const c4a_result = initialBufferFunction(c4_agreementOperation0, [
    c4_callback_agreementOperation0,
]);
printOutput("CASE 4a: 1-TO-1 FORWARDING CREATE", c4a_result);
const c4b_result = nextBufferFunction(c4a_result, 2, 3);
printOutput("CASE 4b: 1-TO-1 FORWARDING UPDATE (INCREMENT)", c4b_result);
const c4c_result = nextBufferFunction(c4b_result, 1, 1.5);
printOutput("CASE 4c: 1-TO-1 FORWARDING UPDATE (DECREMENT)", c4c_result);
const c4d_result = nextBufferFunction(c4c_result, 1, 1.5);
printOutput("CASE 4d: 1-TO-1 FORWARDING UPDATE (SAME)", c4d_result);
const c4e = nextBufferFunction(c4d_result, 0);
printOutput("CASE 4e: 1-TO-1 FORWARDING DELETE", c4e);

// next steps:
// available balance and law/rule of negative balance leading to jailing (can't return funds to user)
// - would be an interesting exercise to think of ways to be able to plug in rules which get checked
// -
// we also need to include whether the receiver is an app as behavior changes because of this
