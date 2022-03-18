// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "../../superfluid/Superfluid.sol";
import "../../superfluid/SuperToken.sol";
import "../../agreements/ConstantFlowAgreementV1.sol";
import "../../agreements/InstantDistributionAgreementV1.sol";
import "../../apps/CFAv1Library.sol";
import "../../apps/IDAv1Library.sol";

contract SuperfluidTester {

    CFAv1Library.InitData private _cfaLib;
    IDAv1Library.InitData private _idaLib;
    IERC20 private _token;
    ISuperToken private _superToken;

    using CFAv1Library for CFAv1Library.InitData;
    using IDAv1Library for IDAv1Library.InitData;

    constructor (
        Superfluid host,
        IConstantFlowAgreementV1 cfa,
        IInstantDistributionAgreementV1 ida,
        IERC20 token,
        ISuperToken superToken) {
        _cfaLib.host = host;
        _cfaLib.cfa = cfa;
        _idaLib.host = host;
        _idaLib.ida = ida;
        _token = token;
        _superToken = superToken;
    }

    function upgradeSuperToken(uint256 amount) external {
        _token.approve(address(_superToken), amount);
        _superToken.upgrade(amount);
    }

    function downgradeSuperToken(uint256 amount) external {
        _superToken.downgrade(amount);
    }

    function flow(address receiver, int96 flowRate) external {
        _cfaLib.flow(receiver, _superToken, flowRate);
    }

    function createIndex(uint32 indexId) external {
        _idaLib.createIndex(_superToken, indexId);
    }

    function updateSubscriptionUnits(uint32 indexId, address subscriber, uint128 units) external {
        _idaLib.updateSubscriptionUnits(_superToken, indexId, subscriber, units);
    }

    function distribute(uint32 indexId, uint256 amount) external {
        _idaLib.distribute(_superToken, indexId, amount);
    }

    function approveSubscription(address publisher, uint32 indexId) external {
        _idaLib.approveSubscription(_superToken, publisher, indexId);
    }

}
