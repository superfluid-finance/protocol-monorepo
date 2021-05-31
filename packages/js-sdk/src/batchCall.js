const { getErrorResponse } = require("./utils/error");
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
        throw `You did not provide an amount for item #${index} in your batch call array.`;
    if (!token)
        throw `You did not provide the token for item #${index} in your batch call array.`;

    /**
     * @dev ERC20.approve batch operation type
     * Call spec:
     * ISuperToken(target).operationApprove(
     *     abi.decode(data, (address spender, uint256 amount))
     * )
     */
    if (operationType === OPERATION_TYPES.ERC20_APPROVE) {
        if (!spender)
            throw `You did not provide the spender for item #${index} in your batch call array.`;
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
    if (!spender)
        throw `You did not provide the sender for item #${index} in your batch call array.`;
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
        throw `You did not provide an amount for item #${index} in your batch call array.`;
    if (!token)
        throw `You did not provide the token for item #${index} in your batch call array.`;

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
            throw `You did not provide the agreementType for item #${index} in your batch call array.`;
        if (!method)
            throw `You did not provide the method for item #${index} in your batch call array.`;
        if (!arguments)
            throw `You did not provide any arguments for item #${index} in your batch call array.`;
        if (!OBJECT.keys(AGREEMENT_TYPES).includes(agreementType))
            throw `You provided an invalid agreementType for item #${index} in your batch call array.`;

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
        throw `You did not provide a superApp for item #${index} in your batch call array.`;
    // TODO: update  callData error
    // if (!callData)
    //     throw `You did not provide the callData for item #${index} in your batch call array.`;
    return [
        operationType,
        superApp,
        // Open-ended? Use existing helper library?
        abiCoder.encode(["uint256"], [amount]),
    ];
};

const parse = ({ index, type, data }) => {
    try {
        if (!type)
            throw `You did not provide a type for item #${index} in your batch call array.`;
        if (!data)
            throw `You did not provide any data for item #${index} in your batch call array.`;

        // Opertation type
        let operationType = type;
        if (typeof type !== Number) {
            if (!Object.keys(OPERATION_TYPES).includes(type))
                throw `You provided an invalid operation type "${type}" for item #${index} in your batch call array. Please see https://docs.superfluid.finaince/bathcall for a list of available types`;
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
        throw `You provided an invalid operation type "${type}" for item #${index} in your batch call array. Please see https://docs.superfluid.finaince/bathcall for a list of available types`;
    } catch (e) {
        throw getErrorResponse(e, "batchCall");
    }
};

const batchCall = ({ sf, calls }) => {
    if (!calls)
        throw getErrorResponse(
            "You must provide an array of calls",
            "batchCall"
        );
    const parsedCalls = calls.map((call, index) => parse({ index, ...call }));
    return sf.host.batchCall(parsedCalls);
};

module.exports = { batchCall };
