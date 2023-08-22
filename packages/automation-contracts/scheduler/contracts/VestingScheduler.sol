// SPDX-License-Identifier: AGPLv3
// solhint-disable not-rely-on-time
pragma solidity ^0.8.0;
import {
    ISuperfluid, ISuperToken, SuperAppDefinitions, IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { SuperAppBase } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { IVestingScheduler } from "./interface/IVestingScheduler.sol";

contract VestingScheduler is IVestingScheduler, SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;
    mapping(bytes32 => VestingSchedule) public vestingSchedules; // id = keccak(supertoken, sender, receiver)

    uint32 public constant MIN_VESTING_DURATION = 7 days;
    uint32 public constant START_DATE_VALID_AFTER = 3 days;
    uint32 public constant END_DATE_VALID_BEFORE = 1 days;

    constructor(ISuperfluid host, string memory registrationKey) {
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
        host.registerAppWithKey(configWord, registrationKey);
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
        newCtx = ctx;
        address sender = _getSender(ctx);

        if (receiver == address(0) || receiver == sender) revert AccountInvalid();
        if (address(superToken) == address(0)) revert ZeroAddress();
        if (flowRate <= 0) revert FlowRateInvalid();
        if (cliffDate != 0 && startDate > cliffDate) revert TimeWindowInvalid();
        if (cliffDate == 0 && cliffAmount != 0) revert CliffInvalid();

        uint32 cliffAndFlowDate = cliffDate == 0 ? startDate : cliffDate;
        if (cliffAndFlowDate <= block.timestamp ||
            cliffAndFlowDate >= endDate ||
            cliffAndFlowDate + START_DATE_VALID_AFTER >= endDate - END_DATE_VALID_BEFORE ||
            endDate - cliffAndFlowDate < MIN_VESTING_DURATION
        ) revert TimeWindowInvalid();

        bytes32 hashConfig = keccak256(abi.encodePacked(superToken, sender, receiver));
        if (vestingSchedules[hashConfig].endDate != 0) revert ScheduleAlreadyExists();
        vestingSchedules[hashConfig] = VestingSchedule(
            cliffAndFlowDate,
            endDate,
            flowRate,
            cliffAmount
        );

        emit VestingScheduleCreated(
            superToken,
            sender,
            receiver,
            startDate,
            cliffDate,
            flowRate,
            endDate,
            cliffAmount
        );
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
        bytes32 configHash = keccak256(abi.encodePacked(superToken, sender, receiver));
        VestingSchedule memory schedule = vestingSchedules[configHash];

        if (schedule.cliffAndFlowDate > block.timestamp ||
            schedule.cliffAndFlowDate + START_DATE_VALID_AFTER < block.timestamp
        ) revert TimeWindowInvalid();

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

            uint256 earlyEndCompensation = schedule.endDate > block.timestamp ?
                (schedule.endDate - block.timestamp) * uint96(schedule.flowRate) : 0;
            bool didCompensationFail;
            if (earlyEndCompensation != 0) {
                // try-catch this because if the account does not have tokens for earlyEndCompensation
                // we should delete the flow anyway.
                try superToken.transferFrom(sender, receiver, earlyEndCompensation)
                // solhint-disable-next-line no-empty-blocks
                {} catch {
                    didCompensationFail = true;
                }
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
