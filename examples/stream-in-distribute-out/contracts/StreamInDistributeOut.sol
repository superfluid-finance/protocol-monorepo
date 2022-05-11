// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.13;

import {IConstantFlowAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    ISuperToken,
    ISuperfluid,
    SuperAppBase,
    SuperAppDefinitions
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";
import {
    IInstantDistributionAgreementV1,
    IDAv1Library
} from "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";

error InvalidToken();
error InvalidAgreement();
error Unauthorized();

/// @title Abstract contract to stream into and distribute out.
/// @notice Users stream in and receive a proportional amount of shares. The shares represent a
/// percentage of a distribution, which gets called in the `executeAction` function.
/// @dev Inheriting contracts MUST implement `_beforeAction()` and `_afterAction()` in inheriting
/// contracts.
abstract contract StreamInDistributeOut is SuperAppBase {

    /// @dev IDAv1Library for brevity.
    using IDAv1Library for IDAv1Library.InitData;
    IDAv1Library.InitData internal _idaLib;

    /// @dev Superfluid Contracts.
    ISuperfluid internal immutable _host;
    IConstantFlowAgreementV1 internal immutable _cfa;

    /// @dev SuperToken to stream in.
    ISuperToken internal immutable _inToken;

    /// @dev SuperToken to distribute out.
    ISuperToken internal immutable _outToken;

    /// @dev Index ID for the distribution.
    uint32 internal constant INDEX_ID = 0;

    /// @dev Checks every callback to validate inputs. MUST be called by the host.
    /// @param agreementClass The agreement address being called. MUST be the cfa.
    /// @param token The Super Token streamed in. MUST be the in-token.
    modifier checkCallback(address agreementClass, ISuperToken token) {
        if (agreementClass != address(_cfa)) revert InvalidAgreement();
        if (token != _inToken) revert InvalidToken();
        if (msg.sender != address(_host)) revert Unauthorized();
        _;
    }

    constructor(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        IInstantDistributionAgreementV1 ida,
        ISuperToken inToken,
        ISuperToken outToken
    ) {
        _idaLib = IDAv1Library.InitData(host, ida);
        _host = host;
        _cfa = cfa;
        _inToken = inToken;
        _outToken = outToken;

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL
            | SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP
            // | SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP
            | SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP
            // | SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP
            | SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;
            // | SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;

        host.registerApp(configWord);

        _idaLib.createIndex(_outToken, INDEX_ID);
    }

    /// @notice Executes dev-defined action and distributes the out-token.
    /// @dev DO NOT override this function, override `_beforeAction` and `_afterAction` instead.
    function executeAction() external {
        uint256 distributionAmount = _beforeAction();
        _idaLib.distribute(_outToken, INDEX_ID, distributionAmount);
        _afterAction();
    }

    /// @dev Callback executed AFTER a stream is CREATED.
    /// @param token Super Token being streamed in.
    /// @param agreementClass Agreement contract address.
    /// @param agreementId Unique stream ID for fetching the flowRate.
    /// @param ctx Callback context.
    function afterAgreementCreated(
        ISuperToken token,
        address agreementClass, 
        bytes32 agreementId,
        bytes calldata,
        bytes calldata,
        bytes calldata ctx
    ) external override checkCallback(agreementClass, token) returns (bytes memory) {
        address sender = _host.decodeCtx(ctx).msgSender;
        (,int96 flowRate,,) = _cfa.getFlowByID(token, agreementId);

        return _idaLib.updateSubscriptionUnitsWithCtx(
            ctx,
            _outToken,
            INDEX_ID,
            sender,
            uint128(int128(flowRate))
        );
    }

    /// @dev Callback executed AFTER a stream is UPADTED.
    /// @param token Super Token being streamed in.
    /// @param agreementClass Agreement contract address.
    /// @param agreementId Unique stream ID for fetching the flowRate.
    /// @param ctx Callback context.
    function afterAgreementUpdated(
        ISuperToken token,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata,
        bytes calldata,
        bytes calldata ctx
    ) external override checkCallback(agreementClass, token) returns (bytes memory newCtx) {
        address sender = _host.decodeCtx(ctx).msgSender;
        (,int96 flowRate,,) = _cfa.getFlowByID(token, agreementId);

        return _idaLib.updateSubscriptionUnitsWithCtx(
            ctx,
            _outToken,
            INDEX_ID,
            sender,
            uint128(int128(flowRate))
        );
    }

    /// @dev Callback executed AFTER a stream is TERMINATED.
    /// @param token Super Token no longer being streamed in.
    /// @param agreementClass Agreement contract address.
    /// @param ctx Callback context.
    function afterAgreementTerminated(
        ISuperToken token,
        address agreementClass,
        bytes32,
        bytes calldata,
        bytes calldata,
        bytes calldata ctx
    ) external override checkCallback(agreementClass, token) returns (bytes memory) {
        address sender = _host.decodeCtx(ctx).msgSender;

        return _idaLib.deleteSubscriptionWithCtx(
            ctx,
            _outToken,
            address(this),
            INDEX_ID,
            sender
        );
    }

    // Executes dev-defined action BEFORE the out-token distribution.
    function _beforeAction() internal virtual returns (uint256 distributionAmount);

    // Executes dev-defined action AFTER the out-token distribution.
    function _afterAction() internal virtual;
}
