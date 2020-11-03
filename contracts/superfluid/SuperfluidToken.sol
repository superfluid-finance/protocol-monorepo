// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.3;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";
import { ISuperfluidGovernance } from "../interfaces/superfluid/ISuperfluidGovernance.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";

import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/SafeCast.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { FixedSizeData } from "../utils/FixedSizeData.sol";


/**
 * @title Superfluid's token implementation
 *
 * @author Superfluid
 */
abstract contract SuperfluidToken is ISuperfluidToken
{

    using SafeMath for uint256;
    using SafeCast for uint256;
    using SignedSafeMath for int256;

    /// @dev Superfluid contract
    ISuperfluid internal _host;

    /// @dev Active agreement bitmap
    mapping(address => uint256) internal _inactiveAgreementBitmap;

    /// @dev Settled balance for the account
    mapping(address => int256) internal _balances;

    /// @dev ISuperfluidToken.getAccountActiveAgreements implementation
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

    /// @dev ISuperfluidToken.isAccountInsolvent implementation
    function isAccountInsolvent(
       address account
    )
       external view override
       returns(bool)
    {
       (int256 amount, ,) = realtimeBalanceOf(account, block.timestamp);
       return amount < 0;
    }

    /// @dev ISuperfluidToken.realtimeBalanceOf implementation
    function realtimeBalanceOf(
       address account,
       uint256 timestamp
    )
       public view override
       returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit)
    {
        int256 realtimeBalance = _balances[account];
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
            realtimeBalance = realtimeBalance.add(agreementDynamicBalance);
            deposit = deposit.add(agreementDeposit);
            owedDeposit = owedDeposit.add(agreementOwedDeposit);
        }
        // Available Balance = Realtime Balance - Max(0, Deposit - OwedDeposit)
        availableBalance = realtimeBalance.sub(
            deposit > owedDeposit ?
            (deposit - owedDeposit).toInt256() : 0);
    }

    /// @dev ISuperfluidToken.realtimeBalanceOfNow implementation
    function realtimeBalanceOfNow(
       address account
    )
       external view override
       returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit) {
       return realtimeBalanceOf(account, block.timestamp);
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
        int256 amount
    )
        internal
    {
        _balances[account] = _balances[account].add(amount);
    }

    function _burn(
        address account,
        int256 amount
    )
        internal
    {
        (int256 availableBalance,,) = realtimeBalanceOf(account, block.timestamp);
        require(availableBalance >= amount, "SuperfluidToken: burn amount exceeds balance");
        _balances[account] = _balances[account].sub(amount);
    }

    function _move(
        address from,
        address to,
        int256 amount
    )
        internal
    {
        (int256 availableBalance,,) = realtimeBalanceOf(from, block.timestamp);
        require(availableBalance >= amount, "SuperfluidToken: move amount exceeds balance");
        _balances[from] = _balances[from].sub(amount);
        _balances[to] = _balances[to].add(amount);
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
        // FIXME change how this is done
        //_addAgreementClass(msg.sender, account);
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
        _balances[account] = _balances[account].add(delta);
    }

    /// @dev ISuperfluidToken.liquidateAgreement implementation
    function liquidateAgreement
    (
        address liquidator,
        bytes32 id,
        address account,
        uint256 singleDeposit,
        uint256 totalDeposit
    )
        external override
        onlyAgreement
    {
        require(totalDeposit >= singleDeposit, "SuperfluidToken: invalid single deposit");

        ISuperfluidGovernance gov = _host.getGovernance();
        address rewardAccount = gov.getRewardAddress(this);

        // reward go to liquidator if reward address is null
        if (rewardAccount == address(0)) {
            rewardAccount = liquidator;
        }

        int256 signedSingleDeposit = singleDeposit.toInt256();
        int256 signedTotalDeposit = totalDeposit.toInt256();

        (int256 availableBalance, , ) = realtimeBalanceOf(account, block.timestamp);

        // Liquidation rules:
        //    - let Available Balance = AB (is negative)
        //    -     Agreement Single Deposit = SD
        //    -     Agreement Total Deposit = TD
        //    -     Total Reward Left = RL = AB + TD
        // #1 Can the total account deposit can still cover the available balance deficit?
        int256 totalRewardLeft = availableBalance.add(signedTotalDeposit);
        if (totalRewardLeft > 0) {
            // #1.a.1 yes: then reward = (SD / TD) * RL
            int256 reward = signedSingleDeposit.mul(totalRewardLeft).div(signedTotalDeposit);
            // #1.a.2 reward go to reward account
            _balances[rewardAccount] = _balances[rewardAccount].add(reward);
            _balances[account] = _balances[account].sub(reward);
            emit AgreementLiquidated(msg.sender, id, account, rewardAccount, uint256(reward));
        } else {
            // #1.b.1 no: then the liquidator takes full amount of the single deposit
            _balances[liquidator] = _balances[liquidator].add(signedSingleDeposit);
            // #1.b.2 account is refunded with the rest of the defict (-RL)
            _balances[account] = _balances[account]
                .sub(totalRewardLeft /* <= 0 */);
            // #1.b.3 the reward address pay the deficit
            //        and the account pays for the full amount of the single deposit
            _balances[rewardAccount] = _balances[rewardAccount]
                .add(totalRewardLeft /* <= 0 */)
                .sub(signedSingleDeposit /* > 0 */);
            emit AgreementLiquidated(msg.sender, id, account, liquidator, uint256(signedSingleDeposit));
            emit Bailout(rewardAccount, uint256(totalRewardLeft));
        }
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

}
