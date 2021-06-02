const {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText,
} = require("./utils/error");
const { AbiCoder } = require("@ethersproject/abi");

const abiCoder = new AbiCoder();

const OPERATION_TYPES = {
    ERC20_APPROVE: 1,
    ERC20_TRANSFER_FROM: 2,
    SUPERTOKEN_UPGRADE: 101,
    SUPERTOKEN_DOWNGRADE: 102,
    SUPERFLUID_CALL_AGREEMENT: 201,
    CALL_APP_ACTION: 202,
};

const AGREEMENT_TYPES = {
    CFA: "cfa",
    IDA: "ida",
};

const parseERC20Operation = ({ index, operationType, data }) => {
    const { token, spender, sender, recipient, amount } = data;
    if (!amount)
        throw getMissingArgumentError("amount", getErrorHelpText(index));
    if (!token) throw getMissingArgumentError("token", getErrorHelpText(index));

    /**
     * @dev ERC20.approve batch operation type
     * Call spec:
     * ISuperToken(target).operationApprove(
     *     abi.decode(data, (address spender, uint256 amount))
     * )
     */
    if (operationType === OPERATION_TYPES.ERC20_APPROVE) {
        if (!spender)
            throw getMissingArgumentError("spender", getErrorHelpText(index));
        return [
            operationType,
            token,
            abiCoder.encode(["address", "uint256"], [spender, amount]),
        ];
    }
    /**
     * @dev ERC20.transferFrom batch operation type
     * Call spec:
     * ISuperToken(target).operationTransferFrom(
     *     abi.decode(data, (address sender, address recipient, uint256 amount)
     * )
     */
    if (!sender)
        throw getMissingArgumentError("sender", getErrorHelpText(index));
    return [
        operationType,
        token,
        abiCoder.encode(
            ["address", "address", "uint256"],
            [sender, recipient, amount]
        ),
    ];
};

const parseSuperTokenOperation = ({ index, operationType, data }) => {
    const { amount, token } = data;
    if (!amount)
        throw getMissingArgumentError("amount", getErrorHelpText(index));
    if (!token) throw getMissingArgumentError("token", getErrorHelpText(index));

    /**
     * @dev SuperToken.upgrade batch operation type
     * Call spec:
     * ISuperToken(target).operationUpgrade(
     *     abi.decode(data, (uint256 amount)
     * )
     */
    /**
     * @dev SuperToken.downgrade batch operation type
     * Call spec:
     * ISuperToken(target).operationDowngrade(
     *     abi.decode(data, (uint256 amount)
     * )
     */
    return [operationType, token, abiCoder.encode(["uint256"], [amount])];
};

const parseSuperFluidOperation = ({ index, operationType, data }) => {
    const {
        superApp,
        agreementType,
        method,
        arguments,
        userData = "0x",
        callData,
    } = data;
    /**
     * @dev Superfluid.callAgreement batch operation type
     * Call spec:
     * callAgreement(
     *     ISuperAgreement(target)),
     *     abi.decode(data, (bytes calldata, bytes userdata)
     * )
     */
    if (operationType === OPERATION_TYPES.SUPERFLUID_CALL_AGREEMENT) {
        if (!agreementType)
            throw getMissingArgumentError(
                "agreementType",
                getErrorHelpText(index)
            );
        if (!method)
            throw getMissingArgumentError("method", getErrorHelpText(index));
        if (!arguments)
            throw getMissingArgumentError("arguments", getErrorHelpText(index));
        if (!OBJECT.keys(AGREEMENT_TYPES).includes(agreementType))
            throw `You provided an invalid agreementType${getBatchCallHelpText(
                index
            )}`;

        const agreementAddress =
            sf.agreements[AGREEMENT_TYPES[agreementType]].address;
        const callData = sf[AGREEMENT_TYPES[agreementType]].contract.methods[
            method
        ](arguments).encodeABI();
        return [
            operationType,
            agreementAddress,
            abiCoder.encode(["bytes", "bytes"], [callData, userData]),
        ];
    }
    /**
     * @dev Superfluid.callAppAction batch operation type
     * Call spec:
     * callAppAction(
     *     ISuperApp(target)),
     *     data
     * )
     */
    if (!superApp)
        throw getMissingArgumentError("superApp", getErrorHelpText(index));
    if (!callData)
        throw getMissingArgumentError("callData", getErrorHelpText(index));
    return [operationType, superApp, callData];
};

const parse = ({ index, type, data }) => {
    try {
        if (!type)
            throw getMissingArgumentError("type", getErrorHelpText(index));
        if (!data)
            throw getMissingArgumentError("data", getErrorHelpText(index));

        // Opertation type
        let operationType = type;
        if (typeof type !== Number) {
            if (!Object.keys(OPERATION_TYPES).includes(type))
                throw `You provided an invalid operation type "${
                    type + 1
                }"${getBatchCallHelpText(index)}`;
            operationType = OPERATION_TYPES[type];
        }

        if (
            [
                OPERATION_TYPES.ERC20_APPROVE,
                OPERATION_TYPES.ERC20_TRANSFER_FROM,
            ].includes(operationType)
        )
            return parseERC20Operation({ index, operationType, data });
        if (
            [
                OPERATION_TYPES.SUPERTOKEN_UPGRADE,
                OPERATION_TYPES.SUPERTOKEN_DOWNGRADE,
            ].includes(operationType)
        )
            return parseSuperTokenOperation({ index, operationType, data });
        if (
            [
                OPERATION_TYPES.SUPERFLUID_CALL_AGREEMENT,
                OPERATION_TYPES.CALL_APP_ACTION,
            ].includes(operationType)
        )
            return parseSuperFluidOperation({ index, operationType, data });
        throw `You provided an invalid operation type "${
            type + 1
        }"${getBatchCallHelpText(index)}`;
    } catch (e) {
        throw getErrorResponse(e, "batchCall");
    }
};

const batchCall = ({ sf, calls }) => {
    console.log(calls);
    if (!calls || typeof calls !== Array)
        throw getErrorResponse(
            "You must provide an array of calls",
            "batchCall"
        );
    const parsedCalls = calls.map((call, index) => parse({ index, ...call }));
    return sf.host.batchCall(parsedCalls);
};

module.exports = { batchCall };
