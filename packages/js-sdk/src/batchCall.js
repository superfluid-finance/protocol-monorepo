const {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText,
} = require("./utils/error");
const {AbiCoder} = require("@ethersproject/abi");

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

const parseERC20Operation = ({index, operationType, data}) => {
    const {token, spender, sender, recipient, amount} = data;
    if (!amount)
        throw new Error(
            getMissingArgumentError("amount", getBatchCallHelpText(index))
        );
    if (!token)
        throw new Error(
            getMissingArgumentError("token", getBatchCallHelpText(index))
        );

    /**
     * @dev ERC20.approve batch operation type
     * Call spec:
     * ISuperToken(target).operationApprove(
     *     abi.decode(data, (address spender, uint256 amount))
     * )
     */
    if (operationType === OPERATION_TYPES.ERC20_APPROVE) {
        if (!spender)
            throw new Error(
                getMissingArgumentError("spender", getBatchCallHelpText(index))
            );
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
        throw new Error(
            getMissingArgumentError("sender", getBatchCallHelpText(index))
        );
    return [
        operationType,
        token,
        abiCoder.encode(
            ["address", "address", "uint256"],
            [sender, recipient, amount]
        ),
    ];
};

const parseSuperTokenOperation = ({index, operationType, data}) => {
    const {amount, token} = data;
    if (!amount)
        throw new Error(
            getMissingArgumentError("amount", getBatchCallHelpText(index))
        );
    if (!token)
        throw new Error(
            getMissingArgumentError("token", getBatchCallHelpText(index))
        );

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

const parseSuperFluidOperation = ({index, operationType, data}) => {
    const {
        superApp,
        agreementType,
        method,
        arguments: args,
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
            throw new Error(
                getMissingArgumentError(
                    "agreementType",
                    getBatchCallHelpText(index)
                )
            );
        if (!method)
            throw new Error(
                getMissingArgumentError("method", getBatchCallHelpText(index))
            );
        if (!args)
            throw new Error(
                getMissingArgumentError(
                    "arguments",
                    getBatchCallHelpText(index)
                )
            );
        if (!Object.keys(AGREEMENT_TYPES).includes(agreementType))
            throw new Error(
                `You provided an invalid agreementType${getBatchCallHelpText(
                    index
                )}`
            );

        const agreementAddress =
            this.agreements[AGREEMENT_TYPES[agreementType]].address;
        const callData = this.agreements[
            AGREEMENT_TYPES[agreementType]
        ].contract.methods[method](...args).encodeABI();
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
        throw new Error(
            getMissingArgumentError("superApp", getBatchCallHelpText(index))
        );
    if (!callData)
        throw new Error(
            getMissingArgumentError("callData", getBatchCallHelpText(index))
        );
    return [operationType, superApp, callData];
};

const parse = ({index, type, data}) => {
    try {
        if (!type)
            throw new Error(
                getMissingArgumentError("type", getBatchCallHelpText(index))
            );
        if (!data)
            throw new Error(
                getMissingArgumentError("data", getBatchCallHelpText(index))
            );

        // Opertation type
        let operationType = type;
        if (typeof type !== Number) {
            if (!Object.keys(OPERATION_TYPES).includes(type))
                throw new Error(
                    `You provided an invalid operation type "${type}"${getBatchCallHelpText(
                        index
                    )}`
                );
            operationType = OPERATION_TYPES[type];
        }

        if (
            [
                OPERATION_TYPES.ERC20_APPROVE,
                OPERATION_TYPES.ERC20_TRANSFER_FROM,
            ].includes(operationType)
        )
            return parseERC20Operation({index, operationType, data});
        if (
            [
                OPERATION_TYPES.SUPERTOKEN_UPGRADE,
                OPERATION_TYPES.SUPERTOKEN_DOWNGRADE,
            ].includes(operationType)
        )
            return parseSuperTokenOperation({index, operationType, data});
        if (
            [
                OPERATION_TYPES.SUPERFLUID_CALL_AGREEMENT,
                OPERATION_TYPES.CALL_APP_ACTION,
            ].includes(operationType)
        )
            return parseSuperFluidOperation({index, operationType, data});
        throw new Error(
            `You provided an invalid operation type "${type}"${getBatchCallHelpText(
                index
            )}`
        );
    } catch (e) {
        throw new Error(getErrorResponse(e, "batchCall"));
    }
};

const batchCall = ({agreements, calls}) => {
    if (!calls || !Array.isArray(calls))
        throw new Error(
            getErrorResponse("You must provide an array of calls", "batchCall")
        );
    this.agreements = agreements;
    return calls.map((call, index) => parse({index, ...call}));
};

module.exports = {batchCall};
