// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";
import { ISuperfluidGovernance } from "../interfaces/superfluid/ISuperfluidGovernance.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";
import { FixedSizeData } from "../libs/FixedSizeData.sol";

/**
 * @title Superfluid's token implementation
 *
 * @author Superfluid
 */
abstract contract SuperfluidToken is ISuperfluidToken
{

    bytes32 private constant _REWARD_ADDRESS_CONFIG_KEY =
        keccak256("org.superfluid-finance.superfluid.rewardAddress");

    using SafeCast for uint256;
    using SafeCast for int256;

    /// @dev Superfluid contract
    ISuperfluid immutable internal _host;

    /// @dev Active agreement bitmap
    mapping(address => uint256) internal _inactiveAgreementBitmap;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev Total supply
    uint256 internal _totalSupply;

    // NOTE: for future compatibility, these are reserved solidity slots
    // The sub-class of SuperfluidToken solidity slot will start after _reserve13
    uint256 internal _reserve4;
    uint256 private _reserve5;
    uint256 private _reserve6;
    uint256 private _reserve7;
    uint256 private _reserve8;
    uint256 private _reserve9;
    uint256 private _reserve10;
    uint256 private _reserve11;
    uint256 private _reserve12;
    uint256 internal _reserve13;

    constructor(
        ISuperfluid host
    ) {
        _host = host;
    }

    /// @dev ISuperfluidToken.getHost implementation
    function getHost()
       external view
       override(ISuperfluidToken)
       returns(address host)
    {
       return address(_host);
    }

    /**************************************************************************
     * Real-time balance functions
     *************************************************************************/

    /// @dev ISuperfluidToken.realtimeBalanceOf implementation
    function realtimeBalanceOf(
       address account,
       uint256 timestamp
    )
       public view override
       returns (
           int256 availableBalance,
           uint256 deposit,
           uint256 owedDeposit)
    {
        availableBalance = _balances[account];
        ISuperAgreement[] memory activeAgreements = getAccountActiveAgreements(account);
        for (uint256 i = 0; i < activeAgreements.length; i++) {
            (
                int256 agreementDynamicBalance,
                uint256 agreementDeposit,
                uint256 agreementOwedDeposit) = activeAgreements[i]
                    .realtimeBalanceOf(
                         this,
                         account,
                         timestamp
                     );
            deposit = deposit + agreementDeposit;
            owedDeposit = owedDeposit + agreementOwedDeposit;
            // 1. Available Balance = Dynamic Balance - Max(0, Deposit - OwedDeposit)
            // 2. Deposit should not be shared between agreements
            availableBalance = availableBalance
                + agreementDynamicBalance
                - (
                    agreementDeposit > agreementOwedDeposit ?
                    (agreementDeposit - agreementOwedDeposit) : 0
                ).toInt256();
        }
    }

    /// @dev ISuperfluidToken.realtimeBalanceOfNow implementation
    function realtimeBalanceOfNow(
       address account
    )
        public view override
        returns (
            int256 availableBalance,
            uint256 deposit,
            uint256 owedDeposit,
            uint256 timestamp)
    {
        timestamp = _host.getNow();
        (
            availableBalance,
            deposit,
            owedDeposit
        ) = realtimeBalanceOf(account, timestamp);
    }

    function isAccountCritical(
        address account,
        uint256 timestamp
    )
        public view override
        returns(bool isCritical)
    {
        (int256 availableBalance,,) = realtimeBalanceOf(account, timestamp);
        return availableBalance < 0;
    }

    function isAccountCriticalNow(
       address account
    )
        external view override
       returns(bool isCritical)
    {
        return isAccountCritical(account, _host.getNow());
    }

    function isAccountSolvent(
        address account,
        uint256 timestamp
    )
        public view override
        returns(bool isSolvent)
    {
        (int256 availableBalance, uint256 deposit, uint256 owedDeposit) =
            realtimeBalanceOf(account, timestamp);
        // Available Balance = Realtime Balance - Max(0, Deposit - OwedDeposit)
        int realtimeBalance = availableBalance
            + (deposit > owedDeposit ? (deposit - owedDeposit) : 0).toInt256();
        return realtimeBalance >= 0;
    }

    function isAccountSolventNow(
       address account
    )
       external view override
       returns(bool isSolvent)
    {
        return isAccountSolvent(account, _host.getNow());
    }

    /// @dev ISuperfluidToken.getAccountActiveAgreements implementation
    function getAccountActiveAgreements(address account)
       public view override
       returns(ISuperAgreement[] memory)
    {
       return _host.mapAgreementClasses(~_inactiveAgreementBitmap[account]);
    }

    /**************************************************************************
     * Token implementation helpers
     *************************************************************************/

    function _mint(
        address account,
        uint256 amount
    )
        internal
    {
        _balances[account] = _balances[account] + amount.toInt256();
        _totalSupply = _totalSupply + amount;
    }

    function _burn(
        address account,
        uint256 amount
    )
        internal
    {
        (int256 availableBalance,,) = realtimeBalanceOf(account, _host.getNow());
        require(availableBalance >= amount.toInt256(), "SuperfluidToken: burn amount exceeds balance");
        _balances[account] = _balances[account] - amount.toInt256();
        _totalSupply = _totalSupply - amount;
    }

    function _move(
        address from,
        address to,
        int256 amount
    )
        internal
    {
        (int256 availableBalance,,) = realtimeBalanceOf(from, _host.getNow());
        require(availableBalance >= amount, "SuperfluidToken: move amount exceeds balance");
        _balances[from] = _balances[from] - amount;
        _balances[to] = _balances[to] + amount;
    }

    /**************************************************************************
     * Super Agreement hosting functions
     *************************************************************************/

    /// @dev ISuperfluidToken.createAgreement implementation
    function createAgreement(
        bytes32 id,
        bytes32[] calldata data
    )
        external override
    {
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        require(!FixedSizeData.hasData(slot, data.length), "SuperfluidToken: agreement already created");
        FixedSizeData.storeData(slot, data);
        emit AgreementCreated(agreementClass, id, data);
    }

    /// @dev ISuperfluidToken.getAgreementData implementation
    function getAgreementData(
        address agreementClass,
        bytes32 id,
        uint dataLength
    )
        external view override
        returns(bytes32[] memory data)
    {
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        data = FixedSizeData.loadData(slot, dataLength);
    }

    /// @dev ISuperfluidToken.updateAgreementData implementation
    function updateAgreementData(
        bytes32 id,
        bytes32[] calldata data
    )
        external override
    {
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        FixedSizeData.storeData(slot, data);
        emit AgreementUpdated(msg.sender, id, data);
    }

    /// @dev ISuperfluidToken.terminateAgreement implementation
    function terminateAgreement(
        bytes32 id,
        uint dataLength
    )
        external override
    {
        address agreementClass = msg.sender;
        bytes32 slot = keccak256(abi.encode("AgreementData", agreementClass, id));
        require(FixedSizeData.hasData(slot,dataLength), "SuperfluidToken: agreement does not exist");
        FixedSizeData.eraseData(slot, dataLength);
        emit AgreementTerminated(msg.sender, id);
    }

    /// @dev ISuperfluidToken.updateAgreementState implementation
    function updateAgreementStateSlot(
        address account,
        uint256 slotId,
        bytes32[] calldata slotData
    )
        external override
    {
        bytes32 slot = keccak256(abi.encode("AgreementState", msg.sender, account, slotId));
        FixedSizeData.storeData(slot, slotData);
        emit AgreementStateUpdated(msg.sender, account, slotId);
    }

    /// @dev ISuperfluidToken.getAgreementState implementation
    function getAgreementStateSlot(
        address agreementClass,
        address account,
        uint256 slotId,
        uint dataLength
    )
        external override view
        returns (bytes32[] memory slotData) {
        bytes32 slot = keccak256(abi.encode("AgreementState", agreementClass, account, slotId));
        slotData = FixedSizeData.loadData(slot, dataLength);
    }

    /// @dev ISuperfluidToken.settleBalance implementation
    function settleBalance(
        address account,
        int256 delta
    )
        external override
        onlyAgreement
    {
        _balances[account] = _balances[account] + delta;
    }

    /// @dev ISuperfluidToken.makeLiquidationPayoutsV2 implementation
    function makeLiquidationPayoutsV2(
        bytes32 id,
        bytes memory liquidationTypeData,
        address liquidatorAccount, // the address executing the liquidation
        bool useDefaultRewardAccount,
        address targetAccount, // the flow sender
        uint256 rewardAmount,
        int256 targetAccountBalanceDelta
    ) external override onlyAgreement {
        address rewardAccount;
        address defaultRewardAccount;

        {
            ISuperfluidGovernance gov = _host.getGovernance();
            defaultRewardAccount = gov.getConfigAsAddress(_host, this, _REWARD_ADDRESS_CONFIG_KEY);
            rewardAccount = defaultRewardAccount;
        }

        // we set the rewardAccount to the user who executed the liquidation if
        // no rewardAccount is set (ANARCHY MODE - should not occur in reality, for testing purposes)
        if (defaultRewardAccount == address(0)) {
            rewardAccount = liquidatorAccount;
        }

        if (useDefaultRewardAccount) {
            _balances[rewardAccount] = _balances[rewardAccount]
                + rewardAmount.toInt256();
        } else {
            _balances[liquidatorAccount] = _balances[liquidatorAccount]
                + rewardAmount.toInt256();

            // this can occur in two cases:
            // - pleb period: the two amounts cancel each other out
            // - pirate/bailout period: reward account has to pay bailout amount
            _balances[rewardAccount] = _balances[rewardAccount]
                - rewardAmount.toInt256()
                - targetAccountBalanceDelta;
        }

        // if targetAccountBalanceDelta > 0, it is a bailout, else a solvent liquidation
        _balances[targetAccount] = _balances[targetAccount]
            + targetAccountBalanceDelta;

        emit AgreementLiquidatedV2(
            msg.sender,
            id,
            liquidatorAccount,
            targetAccount,
            useDefaultRewardAccount ? rewardAccount : liquidatorAccount,
            rewardAmount,
            targetAccountBalanceDelta,
            liquidationTypeData
        );
    }
    /**************************************************************************
    * Modifiers
    *************************************************************************/

    modifier onlyAgreement() {
        require(
            _host.isAgreementClassListed(ISuperAgreement(msg.sender)),
            "SuperfluidToken: only listed agreeement");
        _;
    }

    modifier onlyHost() {
        require(address(_host) == msg.sender, "SuperfluidToken: Only host contract allowed");
        _;
    }

}
