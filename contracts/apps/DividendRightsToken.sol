// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

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

    // use callbacks to track approved subscriptions
    mapping (address => bool) public isSubscribing;

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

    function beforeAgreementCreated(
        ISuperToken superToken,
        bytes calldata /*ctx*/,
        address agreementClass,
        bytes32 /* agreementId */
    )
        external view override
        returns (bytes memory /* data */)
    {
        require(superToken == _cashToken, "DRT: Unsupported cash token");
        require(agreementClass == address(_ida), "DRT: Unsupported agreement");
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address /* agreementClass */,
        bytes32 agreementId,
        bytes calldata /*cbdata*/
    )
        external override
        returns(bytes memory newCtx)
    {
        _checkSubscription(superToken, ctx, agreementId);
        newCtx = ctx;
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        bytes calldata /*ctx*/,
        address agreementClass,
        bytes32 /* agreementId */
    )
        external view override
        returns (bytes memory /* data */)
    {
        require(superToken == _cashToken, "DRT: Unsupported cash token");
        require(agreementClass == address(_ida), "DRT: Unsupported agreement");
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address /* agreementClass */,
        bytes32 agreementId,
        bytes calldata /*cbdata*/
    )
        external override
        returns(bytes memory newCtx)
    {
        _checkSubscription(superToken, ctx, agreementId);
        newCtx = ctx;
    }

    function _checkSubscription(
        ISuperToken superToken,
        bytes calldata ctx,
        bytes32 agreementId
    )
        private
    {
        (bytes4 agreementSelector,,address subscriber,,) = _host.decodeCtx(ctx);
        // only interested in the subscription approval callbacks
        if (agreementSelector == IInstantDistributionAgreementV1.approveSubscription.selector) {
            address publisher;
            uint32 indexId;
            bool approved;
            uint128 units;
            uint256 pendingDistribution;
            (publisher, indexId, approved, units, pendingDistribution) =
                _ida.getSubscriptionByID(superToken, agreementId);

            // sanity checks for testing purpose
            require(publisher == address(this), "DRT: publisher mismatch");
            require(indexId == INDEX_ID, "DRT: publisher mismatch");

            if (approved) {
                isSubscribing[subscriber] = true;
            }
        }
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
        (uint256 actualCashAmount,) = _ida.calculateDistribution(
            _cashToken,
            address(this), INDEX_ID,
            cashAmount);

        _cashToken.transferFrom(_owner, address(this), actualCashAmount);

        _host.callAgreement(
            _ida,
            abi.encodeWithSelector(
                _ida.distribute.selector,
                _cashToken,
                INDEX_ID,
                actualCashAmount,
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
