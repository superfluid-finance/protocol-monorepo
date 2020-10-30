// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.3;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";
import { ISuperfluidGovernance } from "../interfaces/superfluid/ISuperfluidGovernance.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";

import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { FixedSizeData } from "../utils/FixedSizeData.sol";
import { SafeMathExtra } from "../utils/SafeMathExtra.sol";


/**
 * @title Superfluid's token implementation
 *
 * @author Superfluid
 */
abstract contract SuperfluidToken is ISuperfluidToken
{

    using SafeMath for uint256;
    using SafeMathExtra for uint256;
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
            (deposit - owedDeposit).downcastINT256() : 0);
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

    /// @dev ISuperfluidToken.liquidateAgreement implementation
    function liquidateAgreement
    (
        address liquidator,
        bytes32 id,
        address account,
        uint256 deposit
    )
        external override
        onlyAgreement
    {
        ISuperfluidGovernance gov = _host.getGovernance();
        address rewardAccount = gov.getRewardAddress(this);

        // reward go to liquidator if reward address is null
        if (rewardAccount == address(0)) {
            rewardAccount = liquidator;
        }

        int256 signedDeposit = deposit.downcastINT256();
        (int256 availableBalance, uint256 totalDepsit, ) = realtimeBalanceOf(account, block.timestamp);

        // Liquidation rules:
        // #1 Can the agreement deposit can still cover the available balance deficit?
        if (availableBalance.add(totalDepsit.downcastINT256()) > 0) {
            // FIXME reward should be proportional to totalDeposit
            // #1.a.1 yes: then the reward address takes the deposit (minus account deficit refund)
            int256 reward = signedDeposit;
            _balances[rewardAccount] = _balances[rewardAccount].add(reward);
            // #1.a.2 the account pays for the reward
            _balances[account] = 0;
            emit AgreementLiquidated(msg.sender, id, account, rewardAccount, uint256(reward));
        } else {
            // #1.b.1 no: then the liquidator takes the deposit
            _balances[liquidator] = _balances[liquidator].add(signedDeposit);
            // #1.b.2 the account still pays for the deposit, but also refunded with the deficit
            _balances[account] = _balances[account]
                .sub(availableBalance)
                .sub(signedDeposit);
            // #1.b.3 and the reward address pay the deficit
            _balances[rewardAccount] = _balances[rewardAccount].add(availableBalance);
            emit AgreementLiquidated(msg.sender, id, account, liquidator, deposit);
        }
    }

    function settleBalance(
        address account,
        int256 delta
    )
        external override
        onlyAgreement
    {
        _balances[account] = _balances[account].add(delta);
    }

    /**************************************************************************
    * Modifiers
    *************************************************************************/

    modifier onlyHost() {
        require(address(_host) == msg.sender, "SuperfluidToken: Only host contract allowed");
        _;
    }

    modifier onlyAgreement() {
        require(_host.isAgreementClassListed(ISuperAgreement(msg.sender)), "SF: Only listed agreeement allowed");
        _;
    }

}
