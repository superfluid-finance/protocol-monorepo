// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity ^0.8.0;
import {
    ISuperfluid, ISuperToken, SuperAppDefinitions, IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { SuperAppBase } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { IVestingSchedulerV2 } from "./interface/IVestingSchedulerV2.sol";
import { SafeMath } from "@openzeppelin/contracts/utils/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";

contract VestingSchedulerV2 is IVestingSchedulerV2, SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;
    mapping(bytes32 => VestingSchedule) public vestingSchedules; // id = keccak(supertoken, sender, receiver)

    uint32 public constant MIN_VESTING_DURATION = 7 days;
    uint32 public constant START_DATE_VALID_AFTER = 3 days;
    uint32 public constant END_DATE_VALID_BEFORE = 1 days;

    constructor(ISuperfluid host) {
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                    )
                )
            )
        );
        // Superfluid SuperApp registration. This is a dumb SuperApp, only for front-end tx batch calls.
        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
        SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
        SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
        SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP |
        SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
        host.registerApp(configWord);
    }

    /// @dev IVestingScheduler.createVestingSchedule implementation.
    function createVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = _createVestingSchedule(
            superToken,
            receiver,
            startDate,
            cliffDate,
            flowRate,
            cliffAmount,
            endDate,
            0, // remainderAmount
            ctx
        );
    }

    /// @dev IVestingScheduler.createVestingSchedule implementation.
    function createVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate
    ) external {
        _createVestingSchedule(
            superToken,
            receiver,
            startDate,
            cliffDate,
            flowRate,
            cliffAmount,
            endDate,
            0, // remainderAmount
            bytes("")
        );
    }

    function _createVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate,
        uint256 remainderAmount,
        bytes memory ctx
    ) private returns (bytes memory newCtx) {
        newCtx = ctx;
        address sender = _getSender(ctx);
        
        // Default to current block timestamp if no start date is provided.
        if (startDate == 0) {
            startDate = uint32(block.timestamp);
        }

        // Note: Vesting Scheduler V2 doesn't allow start date to be in the past.
        // V1 did but didn't allow cliff and flow to be in the past though.
        if (startDate < block.timestamp) revert TimeWindowInvalid();

        if (receiver == address(0) || receiver == sender) revert AccountInvalid();
        if (address(superToken) == address(0)) revert ZeroAddress();
        if (flowRate <= 0) revert FlowRateInvalid();
        if (cliffDate != 0 && startDate > cliffDate) revert TimeWindowInvalid();
        if (cliffDate == 0 && cliffAmount != 0) revert CliffInvalid();

        uint32 cliffAndFlowDate = cliffDate == 0 ? startDate : cliffDate;
        // Note: Vesting Scheduler V2 allows cliff and flow to be in the schedule creation block, V1 didn't.
        if (cliffAndFlowDate < block.timestamp ||
            cliffAndFlowDate >= endDate ||
            cliffAndFlowDate + START_DATE_VALID_AFTER >= endDate - END_DATE_VALID_BEFORE ||
            endDate - cliffAndFlowDate < MIN_VESTING_DURATION
        ) revert TimeWindowInvalid();

        bytes32 hashConfig = keccak256(abi.encodePacked(superToken, sender, receiver));
        if (vestingSchedules[hashConfig].endDate != 0) revert ScheduleAlreadyExists();
        vestingSchedules[hashConfig] = VestingSchedule(
            cliffAndFlowDate,
            endDate,
            0 /* claimValidityDate */,
            flowRate,
            false /* isClaimable */,
            cliffAmount,
            remainderAmount
        );

        emit VestingScheduleCreated(
            superToken,
            sender,
            receiver,
            startDate,
            cliffDate,
            flowRate,
            endDate,
            cliffAmount,
            remainderAmount,
            false /* isClaimable */
        );
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod,
        uint32 startDate,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = _createVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            cliffPeriod,
            startDate,
            ctx
        );
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod,
        uint32 startDate
    ) external {
        _createVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            cliffPeriod,
            startDate,
            bytes("")
        );
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod
    ) external {
        _createVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            cliffPeriod,
            0, // startDate
            bytes("")
        );
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration
    ) external {
        _createVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            0, // cliffPeriod
            0, // startDate
            bytes("")
        );
    }

    function _createVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 cliffPeriod,
        uint32 startDate,
        bytes memory ctx
    ) private returns (bytes memory newCtx) {
        // Default to current block timestamp if no start date is provided.
        if (startDate == 0) {
            startDate = uint32(block.timestamp);
        }

        uint32 endDate = startDate + totalDuration;
        int96 flowRate = SafeCast.toInt96(SafeCast.toInt256(totalAmount / totalDuration));
        uint256 remainderAmount = totalAmount - (SafeCast.toUint256(flowRate) * totalDuration);

        if (cliffPeriod == 0) {
            newCtx = _createVestingSchedule(
                superToken, 
                receiver, 
                startDate, 
                0 /* cliffDate */, 
                flowRate, 
                0 /* cliffAmount */, 
                endDate,
                remainderAmount,
                ctx
            );
        } else {
            uint32 cliffDate = startDate + cliffPeriod;
            uint256 cliffAmount = SafeMath.mul(cliffPeriod, SafeCast.toUint256(flowRate));
            newCtx = _createVestingSchedule(
                superToken, 
                receiver, 
                startDate, 
                cliffDate, 
                flowRate, 
                cliffAmount, 
                endDate, 
                remainderAmount,
                ctx
            );
        }
    }

    /// @dev IVestingScheduler.createAndExecuteVestingScheduleFromAmountAndDuration.
    function createAndExecuteVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = _createAndExecuteVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            ctx
        );
    }

    /// @dev IVestingScheduler.createAndExecuteVestingScheduleFromAmountAndDuration.
    function createAndExecuteVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration
    ) external {
        _createAndExecuteVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            bytes("")
        );
    }

    /// @dev IVestingScheduler.createAndExecuteVestingScheduleFromAmountAndDuration.
    function _createAndExecuteVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        bytes memory ctx
    ) private returns (bytes memory newCtx) {
        newCtx = _createVestingScheduleFromAmountAndDuration(
            superToken,
            receiver,
            totalAmount,
            totalDuration,
            0, // cliffPeriod
            0, // startDate
            ctx
        );

        address sender = _getSender(ctx);
        assert(_executeCliffAndFlow(superToken, sender, receiver));
    }

    /// @dev IVestingScheduler.createClaimableVestingSchedule implementation.
    function createClaimableVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = _createClaimableVestingSchedule(
            superToken,
            receiver,
            startDate,
            0 /* claimValidityDate */,
            cliffDate,
            flowRate,
            cliffAmount,
            endDate,
            0 /* remainderAmount */,
            ctx
        );
    }

    /// @dev IVestingScheduler.createClaimableVestingSchedule implementation.
    function createClaimableVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate
    ) external {
        _createClaimableVestingSchedule(
            superToken,
            receiver,
            startDate,
            0 /* claimValidityDate */,
            cliffDate,
            flowRate,
            cliffAmount,
            endDate,
            0 /* remainderAmount */,
            bytes("")
        );
    }

    function _createClaimableVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 claimValidityDate,
        uint32 cliffDate,
        int96 flowRate,
        uint256 cliffAmount,
        uint32 endDate,
        uint256 remainderAmount,
        bytes memory ctx
    ) private returns (bytes memory newCtx) {
        newCtx = ctx;
        address sender = _getSender(ctx);

        // Default to current block timestamp if no start date is provided.
        if (startDate == 0) {
            startDate = uint32(block.timestamp);
        }

        // Note: Vesting Scheduler V2 doesn't allow start date to be in the past.
        // V1 did but didn't allow cliff and flow to be in the past though.
        if (startDate < block.timestamp) revert TimeWindowInvalid();

        if (receiver == address(0) || receiver == sender)
            revert AccountInvalid();
        if (address(superToken) == address(0)) revert ZeroAddress();
        if (flowRate <= 0) revert FlowRateInvalid();
        if (endDate <= END_DATE_VALID_BEFORE) revert TimeWindowInvalid();
        if (cliffDate != 0 && startDate > cliffDate) revert TimeWindowInvalid();
        if (cliffDate == 0 && cliffAmount != 0) revert CliffInvalid();

        uint32 cliffAndFlowDate = cliffDate == 0 ? startDate : cliffDate;

        // If the claim validity date is 0 (meaning no expiry date), set the validity to early end of schedule
        if(claimValidityDate <= 0){
            claimValidityDate = endDate - END_DATE_VALID_BEFORE;
        }
        
        // Note: Vesting Scheduler V2 allows cliff and flow to be in the schedule creation block, V1 didn't.
        if (
            cliffAndFlowDate < block.timestamp ||
            cliffAndFlowDate >= endDate ||
            cliffAndFlowDate + START_DATE_VALID_AFTER >=
            endDate - END_DATE_VALID_BEFORE ||
            endDate - cliffAndFlowDate < MIN_VESTING_DURATION ||
            claimValidityDate < cliffAndFlowDate ||
            claimValidityDate > endDate - END_DATE_VALID_BEFORE
        ) revert TimeWindowInvalid();

        bytes32 hashConfig = keccak256(
            abi.encodePacked(superToken, sender, receiver)
        );
        if (vestingSchedules[hashConfig].endDate != 0)
            revert ScheduleAlreadyExists();
        vestingSchedules[hashConfig] = VestingSchedule(
            cliffAndFlowDate,
            endDate,
            claimValidityDate,
            flowRate,
            true /* isClaimable */,
            cliffAmount,
            remainderAmount
        );

        emit VestingScheduleCreated(
            superToken,
            sender,
            receiver,
            startDate,
            cliffDate,
            flowRate,
            endDate,
            cliffAmount,
            remainderAmount,
            true /* isClaimable */
        );
    }

    /// @dev IVestingScheduler.createClaimableVestingScheduleFromAmountAndDuration implementation.
    function createClaimableVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 claimValidityDate,
        uint32 cliffPeriod,
        uint32 startDate,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        IVestingSchedulerV2.VestingScheduleCreationParam memory params = 
            IVestingSchedulerV2.VestingScheduleCreationParam(
                superToken,
                receiver,
                totalAmount,
                totalDuration,
                cliffPeriod,
                startDate,
                claimValidityDate,
                ctx
            );
        newCtx = _createClaimableVestingScheduleFromAmountAndDuration(params);
    }

    /// @dev IVestingScheduler.createClaimableVestingScheduleFromAmountAndDuration implementation.
    function createClaimableVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 claimValidityDate,
        uint32 cliffPeriod,
        uint32 startDate
    ) external {
        IVestingSchedulerV2.VestingScheduleCreationParam memory params = 
            IVestingSchedulerV2.VestingScheduleCreationParam(
                superToken,
                receiver,
                totalAmount,
                totalDuration,
                cliffPeriod,
                startDate,
                claimValidityDate,
                bytes("")
            );

        _createClaimableVestingScheduleFromAmountAndDuration(params);
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createClaimableVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 claimValidityDate,
        uint32 cliffPeriod
    ) external {
        IVestingSchedulerV2.VestingScheduleCreationParam memory params = 
            IVestingSchedulerV2.VestingScheduleCreationParam(
                superToken,
                receiver,
                totalAmount,
                totalDuration,
                cliffPeriod,
                0, // startDate
                claimValidityDate,
                bytes("")
            );

        _createClaimableVestingScheduleFromAmountAndDuration(params);
    }

    /// @dev IVestingScheduler.createVestingScheduleFromAmountAndDuration implementation.
    function createClaimableVestingScheduleFromAmountAndDuration(
        ISuperToken superToken,
        address receiver,
        uint256 totalAmount,
        uint32 totalDuration,
        uint32 claimValidityDate
    ) external {
        IVestingSchedulerV2.VestingScheduleCreationParam memory params = 
            IVestingSchedulerV2.VestingScheduleCreationParam(
                superToken,
                receiver,
                totalAmount,
                totalDuration,
                0, // cliffPeriod
                0, // startDate
                claimValidityDate,
                bytes("")
            );
        _createClaimableVestingScheduleFromAmountAndDuration(params);
    }

    function _createClaimableVestingScheduleFromAmountAndDuration(
        IVestingSchedulerV2.VestingScheduleCreationParam memory params
    ) private returns (bytes memory newCtx) {
        // Default to current block timestamp if no start date is provided.
        if (params.startDate == 0) {
            params.startDate = uint32(block.timestamp);
        }

        // uint32 endDate = params.startDate + params.totalDuration;
        int96 flowRate = SafeCast.toInt96(
            SafeCast.toInt256(params.totalAmount / params.totalDuration)
        );
        uint256 remainderAmount = params.totalAmount -
            (SafeCast.toUint256(flowRate) * params.totalDuration);

        if (params.cliffPeriod == 0) {
            newCtx = _createClaimableVestingSchedule(
                params.superToken,
                params.receiver,
                params.startDate,
                params.claimValidityDate,
                0 /* cliffDate */,
                flowRate,
                0 /* cliffAmount */,
                params.startDate + params.totalDuration /* calculated endDate */,
                remainderAmount,
                params.ctx
            );
        } else {
            uint256 cliffAmount = SafeMath.mul(
                params.cliffPeriod,
                SafeCast.toUint256(flowRate)
            );
            newCtx = _createClaimableVestingSchedule(
                params.superToken,
                params.receiver,
                params.startDate,
                params.claimValidityDate,
                params.startDate + params.cliffPeriod /* calculated cliffDate */,
                flowRate,
                cliffAmount,
                params.startDate + params.totalDuration /* calculated endDate */,
                remainderAmount,
                params.ctx
            );
        }
    }

    function updateVestingSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 endDate,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = ctx;
        address sender = _getSender(ctx);

        bytes32 configHash = keccak256(abi.encodePacked(superToken, sender, receiver));
        VestingSchedule memory schedule = vestingSchedules[configHash];

        if (endDate <= block.timestamp) revert TimeWindowInvalid();

        // Only allow an update if 1. vesting exists 2. executeCliffAndFlow() has been called
        if (schedule.cliffAndFlowDate != 0 || schedule.endDate == 0) revert ScheduleNotFlowing();
        vestingSchedules[configHash].endDate = endDate;
        // Note: Nullify the remainder amount when complexity of updates is introduced.
        vestingSchedules[configHash].remainderAmount = 0;

        emit VestingScheduleUpdated(
            superToken,
            sender,
            receiver,
            schedule.endDate,
            endDate
        );
    }

    /// @dev IVestingScheduler.deleteVestingSchedule implementation.
    function deleteVestingSchedule(
        ISuperToken superToken,
        address receiver,
        bytes memory ctx
    ) external returns (bytes memory newCtx) {
        newCtx = ctx;
        address sender = _getSender(ctx);
        bytes32 configHash = keccak256(abi.encodePacked(superToken, sender, receiver));

        if (vestingSchedules[configHash].endDate != 0) {
            delete vestingSchedules[configHash];
            emit VestingScheduleDeleted(superToken, sender, receiver);
        } else {
            revert ScheduleDoesNotExist();
        }
    }

    /// @dev IVestingScheduler.executeCliffAndFlow implementation.
    function executeCliffAndFlow(
        ISuperToken superToken,
        address sender,
        address receiver
    ) external returns (bool success) {
        return _executeCliffAndFlow(superToken, sender, receiver);
    }

    /// @dev IVestingScheduler.executeCliffAndFlow implementation.
    function _executeCliffAndFlow(
        ISuperToken superToken,
        address sender,
        address receiver
    ) private returns (bool success) {
        bytes32 configHash = keccak256(abi.encodePacked(superToken, sender, receiver));
        VestingSchedule memory schedule = vestingSchedules[configHash];

        if (schedule.isClaimable) {
            // Ensure that the caller is the sender or the receiver if the vesting schedule requires claiming.
            if (msg.sender != sender && msg.sender != receiver) {
                revert CannotClaimFlowOnBehalf();
            }
            // Ensure that that the claming date is after the cliff/flow date and before the early end of the schedule
            if (schedule.cliffAndFlowDate > block.timestamp ||
                schedule.claimValidityDate < block.timestamp
            ) revert TimeWindowInvalid();
        } else {
            if (schedule.cliffAndFlowDate > block.timestamp ||
                schedule.cliffAndFlowDate + START_DATE_VALID_AFTER < block.timestamp
            ) revert TimeWindowInvalid();
        }

        // Invalidate configuration straight away -- avoid any chance of re-execution or re-entry.
        delete vestingSchedules[configHash].cliffAndFlowDate;
        delete vestingSchedules[configHash].cliffAmount;

        // Compensate for the fact that flow will almost always be executed slightly later than scheduled.
        uint256 flowDelayCompensation = (block.timestamp - schedule.cliffAndFlowDate) * uint96(schedule.flowRate);

        // If there's cliff or compensation then transfer that amount.
        if (schedule.cliffAmount != 0 || flowDelayCompensation != 0) {
            superToken.transferFrom(
                sender,
                receiver,
                schedule.cliffAmount + flowDelayCompensation
            );
        }

        // Create a flow according to the vesting schedule configuration.
        cfaV1.createFlowByOperator(sender, receiver, superToken, schedule.flowRate);

        emit VestingCliffAndFlowExecuted(
            superToken,
            sender,
            receiver,
            schedule.cliffAndFlowDate,
            schedule.flowRate,
            schedule.cliffAmount,
            flowDelayCompensation
        );

        return true;
    }

    /// @dev IVestingScheduler.executeEndVesting implementation.
    function executeEndVesting(
        ISuperToken superToken,
        address sender,
        address receiver
    ) external returns (bool success){
        bytes32 configHash = keccak256(abi.encodePacked(superToken, sender, receiver));
        VestingSchedule memory schedule = vestingSchedules[configHash];

        if (schedule.endDate - END_DATE_VALID_BEFORE > block.timestamp) revert TimeWindowInvalid();

        // Invalidate configuration straight away -- avoid any chance of re-execution or re-entry.
        delete vestingSchedules[configHash];
        // If vesting is not running, we can't do anything, just emit failing event.
        if(_isFlowOngoing(superToken, sender, receiver)) {
            // delete first the stream and unlock deposit amount.
            cfaV1.deleteFlowByOperator(sender, receiver, superToken);

            uint256 earlyEndCompensation = schedule.endDate >= block.timestamp 
                ? (schedule.endDate - block.timestamp) * uint96(schedule.flowRate) + schedule.remainderAmount 
                : 0;

            // Note: we consider the compensation as failed if the stream is still ongoing after the end date.
            bool didCompensationFail = schedule.endDate < block.timestamp;
            if (earlyEndCompensation != 0) {
                // Note: Super Tokens revert, not return false, i.e. we expect always true here.
                assert(superToken.transferFrom(sender, receiver, earlyEndCompensation));
            }

            emit VestingEndExecuted(
                superToken,
                sender,
                receiver,
                schedule.endDate,
                earlyEndCompensation,
                didCompensationFail
            );
        } else {
            emit VestingEndFailed(
                superToken,
                sender,
                receiver,
                schedule.endDate
            );
        }

        return true;
    }

    /// @dev IVestingScheduler.getVestingSchedule implementation.
    function getVestingSchedule(
        address supertoken,
        address sender,
        address receiver
    ) external view returns (VestingSchedule memory) {
        return vestingSchedules[keccak256(abi.encodePacked(supertoken, sender, receiver))];
    }

    /// @dev get sender of transaction from Superfluid Context or transaction itself.
    function _getSender(bytes memory ctx) internal view returns (address sender) {
        if (ctx.length != 0) {
            if (msg.sender != address(cfaV1.host)) revert HostInvalid();
            sender = cfaV1.host.decodeCtx(ctx).msgSender;
        } else {
            sender = msg.sender;
        }
        // This is an invariant and should never happen.
        assert(sender != address(0));
    }

    /// @dev get flowRate of stream
    function _isFlowOngoing(ISuperToken superToken, address sender, address receiver) internal view returns (bool) {
        (,int96 flowRate,,) = cfaV1.cfa.getFlow(superToken, sender, receiver);
        return flowRate != 0;
    }
}
