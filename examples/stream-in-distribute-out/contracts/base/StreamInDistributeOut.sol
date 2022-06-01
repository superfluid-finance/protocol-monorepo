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

// //////////////////////////////////////////////////////////////
// ERRORS
// //////////////////////////////////////////////////////////////

/// @dev Thrown when the wrong token is streamed to the contract.
error InvalidToken();

/// @dev Thrown when the `msg.sender` of the app callbacks is not the Superfluid host.
error Unauthorized();

/// @title Abstract contract to stream into and distribute out.
/// @notice Users stream in and receive a proportional amount of shares. The shares represent a
/// percentage of a distribution, which gets called in the `executeAction` function.
/// @dev Inheriting contracts MUST implement `_beforeDistribution()` in inheriting contracts.
abstract contract StreamInDistributeOut is SuperAppBase {

    // //////////////////////////////////////////////////////////////
    // EVENTS
    // //////////////////////////////////////////////////////////////

    /// @dev Emits when action is successfully executed.
    /// @param distributionAmount Amount that gets distributed to the index.
    event ActionExecuted(uint256 distributionAmount);
    
    /// @dev Emits when action fails in a stream termination callback AND the `amountOwed` can NOT
    /// be transferred back to the address closing the stream.
    /// @param amountOwed Amount owed back to the address that closed the stream.
    event ActionFailed(uint256 amountOwed);

    // //////////////////////////////////////////////////////////////
    // VARIABLES
    // //////////////////////////////////////////////////////////////

    /// @dev Last Distribution timestamp. Used to compute the amount owed to an address that closes
    /// a stream but the `executeAction` call fails.
    uint256 public lastDistribution;

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

    // //////////////////////////////////////////////////////////////
    // MODIFIERS
    // //////////////////////////////////////////////////////////////

    /// @dev Checks every callback to validate inputs. MUST be called by the host.
    /// @param token The Super Token streamed in. MUST be the in-token.
    modifier validCallback(ISuperToken token) {
        if (token != _inToken && token != _outToken) revert InvalidToken();
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

        host.registerApp(
            SuperAppDefinitions.APP_LEVEL_FINAL
                | SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP
                | SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP
                | SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP
        );

        _idaLib.createIndex(outToken, INDEX_ID);
    }

    // //////////////////////////////////////////////////////////////
    // ACTION EXECUTION
    // //////////////////////////////////////////////////////////////

    /// @notice Executes dev-defined action and distributes the out-token.
    /// @dev DO NOT override this function, override `_beforeDistribution` instead.
    function executeAction() public {
        if (!_shouldDistributeHax()) return;

        uint256 distributionAmount = _beforeDistribution();

        _idaLib.distribute(_outToken, INDEX_ID, distributionAmount);

        lastDistribution = block.timestamp;

        emit ActionExecuted(distributionAmount);
    }

    /// @notice Executes dev-defined action and distributes the out-token in a super app callback.
    /// @param ctx Super app callback context byte string.
    /// @return newCtx New context returned from IDA distribution.
    function executeActionInCallback(bytes calldata ctx) public returns (bytes memory newCtx) {
        if (!_shouldDistributeHax()) return ctx;

        uint256 distributionAmount = _beforeDistribution();

        newCtx = _idaLib.distributeWithCtx(ctx, _outToken, INDEX_ID, distributionAmount);

        lastDistribution = block.timestamp;

        emit ActionExecuted(distributionAmount);
    }

    /// @dev Executes dev-defined action BEFORE the out-token distribution.
    /// @return distributionAmount Amount to distribute
    function _beforeDistribution() internal virtual returns (uint256 distributionAmount) {}

    // //////////////////////////////////////////////////////////////
    // SUPER APP CALLBACKS
    // //////////////////////////////////////////////////////////////

    /// @dev Callback executed AFTER a stream is CREATED.
    /// @param token Super Token being streamed in.
    /// @param agreementClass Agreement contract address.
    /// @param agreementId Unique stream ID for fetching the flowRate.
    /// @param ctx Callback context.
    function afterAgreementCreated(
        ISuperToken token,
        address agreementClass, 
        bytes32 agreementId,
        bytes calldata agreementData,
        bytes calldata,
        bytes calldata ctx
    ) external override validCallback(token) returns (bytes memory newCtx) {
        if (agreementClass != address(_cfa)) return ctx;

        newCtx = executeActionInCallback(ctx);

        (address sender, ) = abi.decode(agreementData, (address, address));

        (,int96 flowRate,,) = _cfa.getFlowByID(token, agreementId);

        return _idaLib.updateSubscriptionUnitsWithCtx(
            newCtx,
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
        bytes calldata agreementData,
        bytes calldata,
        bytes calldata ctx
    ) external override validCallback(token) returns (bytes memory newCtx) {
        if (agreementClass != address(_cfa)) return ctx;

        newCtx = executeActionInCallback(ctx);

        (address sender, ) = abi.decode(agreementData, (address, address));

        (,int96 flowRate,,) = _cfa.getFlowByID(token, agreementId);

        return _idaLib.updateSubscriptionUnitsWithCtx(
            ctx,
            _outToken,
            INDEX_ID,
            sender,
            uint128(int128(flowRate))
        );
    }

    /// @dev Callback executed BEFORE a stream is TERMINATED.
    /// @param token Super Token being streamed in
    /// @param agreementId Unique stream ID for fetchign the flowRate and timestamp.
    function beforeAgreementTerminated(
        ISuperToken token,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata,
        bytes calldata
    ) external override view returns (bytes memory) {
        if (agreementClass != address(_cfa)) return new bytes(0);

        (uint256 timestamp, int96 flowRate, , ) = _cfa.getFlowByID(token, agreementId);

        return abi.encode(timestamp, flowRate);
    }

    /// @dev Callback executed AFTER a stream is TERMINATED. This MUST NOT revert.
    /// @param token Super Token no longer being streamed in.
    /// @param agreementClass Agreement contract address.
    /// @param ctx Callback context.
    function afterAgreementTerminated(
        ISuperToken token,
        address agreementClass,
        bytes32,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    ) external override validCallback(token) returns (bytes memory) {
        if (agreementClass != address(_cfa)) return ctx;

        (address sender, ) = abi.decode(agreementData, (address, address));

        // Try to execute the action. On success, continue to `deleteSubscriptionWithCtx`
        try this.executeActionInCallback(ctx) returns (bytes memory newCtx) {
            return _idaLib.deleteSubscriptionWithCtx(
                newCtx,
                _outToken,
                address(this),
                INDEX_ID,
                sender
            );
        } catch {
            // On failure, compute the amount streamed since the last stream update OR last
            // distribution, whichever was most recent, multiply the seconds passed by the
            // flow rate, then transfer that amount out to the address whose stream is being closed.
            // In the event this contract does not hold enough of the input token to refund, an
            // `ActionFailed` event is emitted with the `amountOwed` for offchain refunding.

            // Extract the last flowRate and timestamp before this closure using the `cbdata`
            // encoded in the `beforeAgreementTerminated` callback.
            (, int96 flowRate) = abi.decode(cbdata, (uint256, int96));

            // Compute amount owed by multiplying the number of seconds passed by the flow rate.
            uint256 amountOwed = (block.timestamp - lastDistribution) * uint256(int256(flowRate));

            // If this contract has insufficient balance to refund the address whose stream is being
            // closed, emit `ActionFailed` with the `amountOwed`.
            if (_inToken.balanceOf(address(this)) < amountOwed) emit ActionFailed(amountOwed);

            // Else, we transfer. There should be no case where this reverts, given the last check.
            else _inToken.transfer(sender, amountOwed);

            return _idaLib.deleteSubscriptionWithCtx(
                ctx,
                _outToken,
                address(this),
                INDEX_ID,
                sender
            );
        }
    }

    // Hey, this is me. You're probably wondering how I got here.
    // It all starts with the InstantDistributionAgreementV1 having a bug where `updateIndex` does
    // not break when there are no issued units, however `distribute` throws an 0x12 divide by zero
    // panic. so to avoid this, we check if the units would break. This has been updated in dev and
    // should be in prod soon:tm:. Until this gets updated in prod, though, the hack stays.
    function _shouldDistributeHax() internal view returns (bool) {
        (
            ,
            ,
            uint128 approved,
            uint128 pending
        ) = _idaLib.ida.getIndex(_outToken, address(this), INDEX_ID);

        return pending + approved > 0;
    }
}
