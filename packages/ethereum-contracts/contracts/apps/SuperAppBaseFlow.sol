// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import {ISuperfluid, ISuperToken, ISuperApp, SuperAppDefinitions} from "../interfaces/superfluid/ISuperfluid.sol";
import {SuperTokenV1Library} from "./SuperTokenV1Library.sol";

abstract contract SuperAppBaseFlow is ISuperApp {
    using SuperTokenV1Library for ISuperToken;
    
    ISuperfluid public _host;

    /// @dev Can be used to track accepted status of tokens for Super App
    mapping(ISuperToken => bool) internal _acceptedSuperTokens;

    /// @dev Thrown when the callback caller is not the host.
    error UnauthorizedHost();

    /**
     * @dev Initializes the contract by setting the expected Superfluid Host.
     *      and register which callbacks the Host can engage when appropriate
     */
    constructor(
        ISuperfluid host_,
        bool activateAfterCreatedCallback,
        bool activateAfterUpdatedCallback,
        bool activateAfterDeletedCallback
    ) {
        _host = host_;
        
        uint256 callBackDefinitions = SuperAppDefinitions.APP_LEVEL_FINAL;

        if ( !activateAfterCreatedCallback ) {     
            callBackDefinitions += SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
        }

        if ( !activateAfterUpdatedCallback ) {     
            callBackDefinitions += SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP;
        }

        if ( !activateAfterDeletedCallback ) {     
            callBackDefinitions += SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
        }

        host_.registerApp(callBackDefinitions);
    }

    /**
     * @dev Expect Super Token involved in callback to be an accepted one 
     *      This function can be overridden with custom logic and to revert if desired
     */
    function isAcceptedSuperToken(ISuperToken superToken) public view virtual returns (bool) {
        return _acceptedSuperTokens[superToken];
    }

    /**
     * @dev Expect Super Agreement involved in callback to be an accepted one
     *      This function can be overridden with custom logic and to revert if desired
     *      Current implementation expects ConstantFlowAgreement
     */
    function isAcceptedAgreement(address agreementClass) public view virtual returns (bool) {

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
        if (!isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return ctx;

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
        if (!isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return ctx;

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
        if (!isAcceptedSuperToken(superToken)) return ctx;
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return ctx;

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
        bytes calldata /*ctx*/
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, ) = abi.decode(agreementData, (address, address));

        (uint256 lastUpdated, int96 flowRate,,) = superToken.getFlowInfo(sender, address(this));

        return abi.encode(
            lastUpdated,
            flowRate // will always be zero
        );
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata /*ctx*/
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) revert UnauthorizedHost();
        // super token is accepted
        if (!isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, ) = abi.decode(agreementData, (address, address));

        (uint256 lastUpdated, int96 flowRate,,) = superToken.getFlowInfo(sender, address(this));

        return abi.encode(
            lastUpdated,
            flowRate
        );
    }

    function beforeAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata /*ctx*/
    ) external view override returns (bytes memory /*beforeData*/) {
        // sender is host
        if (msg.sender != address(_host)) return "0x";
        // super token is accepted
        if (!isAcceptedSuperToken(superToken)) return "0x";
        // agreement is CFA
        if (!isAcceptedAgreement(agreementClass)) return "0x";

        (address sender, address receiver) = abi.decode(agreementData, (address, address));

        (uint256 lastUpdated, int96 flowRate,,) = superToken.getFlowInfo(sender, receiver);

        return abi.encode(
            lastUpdated,
            flowRate
        );
    }
}
