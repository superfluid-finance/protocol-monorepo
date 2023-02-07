// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {ISuperfluid, ISuperToken, ISuperApp, SuperAppDefinitions} from "../interfaces/superfluid/ISuperfluid.sol";

abstract contract SuperAppBaseFlow is ISuperApp {
    ISuperfluid public _host;

    mapping(ISuperToken => bool) internal _acceptedSuperTokens;

    /// @dev Thrown when the callback caller is not the host.
    error UnauthorizedHost();

    /**
     * @dev Initializes the contract by setting the expected Superfluid Host.
     *      and register which callbacks the Host can engage when appropriate
     */
    constructor(ISuperfluid host_, uint256 callBackDefinitions) {
        _host = host_;

        host_.registerApp(SuperAppDefinitions.APP_LEVEL_FINAL | callBackDefinitions);
    }

    /**
     * @dev Expect Super Token involved in callback to be an accepted one 
     *      This function can be overridden with custom logic and to revert if desired
     */
    function _isAcceptedSuperToken(ISuperToken superToken) public view virtual returns (bool) {
        return _acceptedSuperTokens[superToken];
    }

    /**
     * @dev Expect Super Agreement involved in callback to be an accepted one
     *      This function can be overridden with custom logic and to revert if desired
     *      Current implementation expects ConstantFlowAgreement
     */
    function _isAcceptedAgreement(address agreementClass) internal view virtual returns (bool) {

        return
            agreementClass ==
            address(
                _host.getAgreementClass(
                    keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                )
            );

    }

    // ---------------------------------------------------------------------------------------------
    // AFTER- VIRTUAL FUNCTIONS

    function afterFlowCreated(
        ISuperToken /*superToken*/,
        address /*sender*/,
        bytes calldata /*beforeData*/,
        bytes calldata /*ctx*/
    ) internal virtual returns (bytes memory /*newCtx*/) {
        revert("Unsupported callback - After Agreement Created");
    }

    function afterFlowUpdated(
        ISuperToken /*superToken*/,
        address /*sender*/,
        bytes calldata /*beforeData*/,
        bytes calldata /*ctx*/
    ) internal virtual returns (bytes memory /*newCtx*/) {
        revert("Unsupported callback - After Agreement Updated");
    }

    function afterFlowDeleted(
        ISuperToken /*superToken*/,
        address /*sender*/,
        address /*receiver*/,
        bytes calldata /*beforeData*/,
        bytes calldata /*ctx*/
    ) internal virtual returns (bytes memory /*newCtx*/) {
        revert("Unsupported callback - After Agreement Deleted");
    }

    // ---------------------------------------------------------------------------------------------
    // BEFORE- VIRTUAL FUNCTIONS

    function beforeFlowCreated(
        ISuperToken /*superToken*/,
        address /*sender*/,
        bytes calldata /*ctx*/
    ) internal view virtual returns (bytes memory /*beforeData*/) {
        revert("Unsupported callback - Before Agreement Created");
    }

    function beforeFlowUpdated(
        ISuperToken /*superToken*/,
        address /*sender*/,
        bytes calldata /*ctx*/
    ) internal view virtual returns (bytes memory /*beforeData*/) {
        revert("Unsupported callback - Before Agreement Updated");
    }

    function beforeFlowDeleted(
        ISuperToken /*superToken*/,
        address /*sender*/,
        address /*receiver*/,
        bytes calldata /*ctx*/
    ) internal view virtual returns (bytes memory /*beforeData*/) {
        revert("Unsupported callback - Before Agreement Deleted");
    }

    // ---------------------------------------------------------------------------------------------
    // AFTER- CALLBACKS

    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return ctx;

        (address sender, ) = abi.decode(agreementData, (address, address));

        return
            afterFlowCreated(
                superToken,
                sender,
                cbdata,
                ctx // userData can be acquired with `_host.decodeCtx(ctx).userData`
            );
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return ctx;

        (address sender, ) = abi.decode(agreementData, (address, address));

        return
            afterFlowUpdated(
                superToken,
                sender,
                cbdata,
                ctx // userData can be acquired with `_host.decodeCtx(ctx).userData`
            );
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        // sender is host
        if (msg.sender != address(_host)) return ctx;
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return ctx;

        (address sender, address receiver) = abi.decode(agreementData, (address, address));

        return 
            afterFlowDeleted(
                superToken,
                sender,
                receiver,
                cbdata,
                ctx
            );
    }

    // ---------------------------------------------------------------------------------------------
    // BEFORE- CALLBACKS

    function beforeAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata ctx
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, ) = abi.decode(agreementData, (address, address));

        return
            beforeFlowCreated(
                superToken,
                sender,
                ctx // userData can be acquired with `host.decodeCtx(ctx).userData`
            );
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata ctx
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, ) = abi.decode(agreementData, (address, address));

        return
            beforeFlowUpdated(
                superToken,
                sender,
                ctx // userData can be acquired with `host.decodeCtx(ctx).userData`
            );
    }

    function beforeAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata ctx
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) return "0x";
        // super token is accepted
        if (!_isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!_isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, address receiver) = abi.decode(agreementData, (address, address));

        return
            beforeFlowDeleted(
                superToken,
                sender,
                receiver,
                ctx // userData can be acquired with `host.decodeCtx(ctx).userData`
            );
    }
}
