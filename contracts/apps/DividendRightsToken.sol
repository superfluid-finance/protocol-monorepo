// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;

import { Ownable } from "../access/Ownable.sol";

import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "./SuperAppBase.sol";
import { IInstantDistributionAgreementV1 } from "../interfaces/agreements/IInstantDistributionAgreementV1.sol";

import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";


contract DividendRightsToken is
    Ownable,
    ERC20,
    SuperAppBase
{

    uint32 public constant INDEX_ID = 0;

    ISuperToken private _cashToken;
    ISuperfluid private _host;
    IInstantDistributionAgreementV1 private _ida;

    constructor(
        string memory name,
        string memory symbol,
        ISuperToken cashToken,
        ISuperfluid host,
        IInstantDistributionAgreementV1 ida)
        ERC20(name, symbol)
    {
        _cashToken = cashToken;
        _host = host;
        _ida = ida;

        uint256 configWord =
            SuperAppDefinitions.TYPE_APP_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);

        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.createIndex.selector,
                _cashToken,
                INDEX_ID,
                new bytes(0)
            )
        );

        _owner = msg.sender;
    }

    /// @dev Issue new `amount` of giths to `beneficiary`
    function issue(address beneficiary, uint256 amount) external onlyOwner {
        // then adjust beneficiary subscription units
        uint256 currentAmount = balanceOf(beneficiary);

        // first try to do ERC20 mint
        ERC20._mint(beneficiary, amount);

        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.updateSubscription.selector,
                _cashToken,
                INDEX_ID,
                beneficiary,
                uint128(currentAmount) + uint128(amount),
                new bytes(0)
            )
        );
    }

    /// @dev Distribute `amount` of cash among all token holders
    function distribute(uint256 cashAmount) external onlyOwner {
        uint128 indexValue;
        uint128 totalUnitsApproved;
        uint128 totalUnitsPending;
        (, indexValue, totalUnitsApproved, totalUnitsPending) = _ida.getIndex(_cashToken, address(this), INDEX_ID);
        uint128 indexDelta = uint128(cashAmount / uint256(totalUnitsApproved + totalUnitsPending));

        // adjust actual cashAmount for imprecision
        cashAmount = uint256(indexDelta) * uint256(totalUnitsApproved + totalUnitsPending);
        _cashToken.transferFrom(_owner, address(this), cashAmount);

        // update the index
        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.updateIndex.selector,
                _cashToken,
                INDEX_ID,
                indexValue + indexDelta,
                new bytes(0)
            )
        );
    }

    /// @dev ERC20._transfer override
    function _transfer(address sender, address recipient, uint256 amount) internal override {
        // first try to do ERC20 transfer
        ERC20._transfer(sender, recipient, amount);

        // then adjust sender and receiver's  subscription units
        uint128 currentUnits;

        (,currentUnits,) = _ida.getSubscription(_cashToken, address(this), INDEX_ID, sender);
        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.updateSubscription.selector,
                _cashToken,
                INDEX_ID,
                sender,
                currentUnits - uint128(amount),
                new bytes(0)
            )
        );

        (,currentUnits,) = _ida.getSubscription(_cashToken, address(this), INDEX_ID, recipient);
        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.updateSubscription.selector,
                _cashToken,
                INDEX_ID,
                recipient,
                currentUnits + uint128(amount),
                new bytes(0)
            )
        );
    }

}
