const { getErrorResponse } = require("./utils/error");

const TYPES = {
    ERC20_APPROVE: 1,
    ERC20_TRANSFER_FROM: 2,
    SUPERTOKEN_UPGRADE: 101,
    SUPERTOKEN_DOWNGRADE: 102,
    SUPERFLUID_CALL_AGREEMENT: 201,
    CALL_APP_ACTION: 202,
};

const parseERC20Operation = ({ index, operationType, data }) => {
    const { tokenAddress, spender, recipient, amount } = data;

    /**
     * @dev ERC20.approve batch operation type
     * Call spec:
     * ISuperToken(target).operationApprove(
     *     abi.decode(data, (address spender, uint256 amount))
     * )
     */
    if (operationType === TYPES.ERC20_APPROVE) {
        return [
            operationType,
            tokenAddress,
            // TODO: Encode params  [spender, amount]
        ];
    }
    /**
     * @dev ERC20.transferFrom batch operation type
     * Call spec:
     * ISuperToken(target).operationTransferFrom(
     *     abi.decode(data, (address sender, address recipient, uint256 amount)
     * )
     */
    return [
        operationType,
        tokenAddress,
        // TODO: Encode params  [sender, recipient, amount]
    ];
};

const parseSuperTokenOperation = ({ index, operationType, data }) => {
    const { amount, tokenAddress } = data;
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
    return [
        operationType,
        tokenAddress,
        // TODO: Encode parameters [amount]
    ];
};

const parseSuperFluidOperation = ({ index, operationType, data }) => {
    /**
     * @dev Superfluid.callAgreement batch operation type
     * Call spec:
     * callAgreement(
     *     ISuperAgreement(target)),
     *     abi.decode(data, (bytes calldata, bytes userdata)
     * )
     */
    // Get CFA or IDA address
    return [
        operationType,
        agreementAddress,
        // TODO: Encode params
        // Open-ended? Use existing helper library?
    ];
    /**
     * @dev Superfluid.callAppAction batch operation type
     * Call spec:
     * callAppAction(
     *     ISuperApp(target)),
     *     data
     * )
     */
    return [
        operationType,
        superAppAddress,
        // TODO: Encode params
        // Open-ended? Use existing helper library?
    ];
};

const parse = ({ index, type, data }) => {
    if (!type) {
        // TODO: throw error
    }
    if (!data) {
        // TODO: throw error
    }
    if (!data.target) {
        // TODO: throw error
    }

    // Opertation type
    let operationType = type;
    if (typeof type !== Number) {
        if (!Object.keys(TYPES).includes(type)) {
            // TODO: throw error
            // `You provided an invalid batch call operation type "${type}" for item #${index} in your call array. Please see https://docs.superfluid.finaince/bathcall for a list of available types`
        }
        operationType = TYPES[type];
    }

    if (
        [TYPES.ERC20_APPROVE, TYPES.ERC20_TRANSFER_FROM].includes(operationType)
    )
        return parseERC20Operation({ index, operationType, data });
    if (
        [TYPES.SUPERTOKEN_UPGRADE, TYPES.SUPERTOKEN_DOWNGRADE].includes(
            operationType
        )
    )
        return parseSuperTokenOperation({ index, operationType, data });
    if (
        [TYPES.SUPERFLUID_CALL_AGREEMENT, TYPES.CALL_APP_ACTION].includes(
            operationType
        )
    )
        return parseSuperFluidOperation({ index, operationType, data });
    // TODO: Update error
    return throw new Error(
        `You provided an invalid batch call operation type "${type}" for item #${index} in your call array. Please see https://docs.superfluid.finaince/bathcall for a list of available types`
    );
};

const batchCall = ({ sf, calls }) => {
    const parsedCalls = calls.map((call, index) => parse({ index, ...call }));
    return sf.host.batchCall(parsedCalls);
};

module.exports = { batchCall };
