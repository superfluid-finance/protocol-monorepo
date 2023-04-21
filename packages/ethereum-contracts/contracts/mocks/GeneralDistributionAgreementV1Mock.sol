// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    ISuperfluid
} from "../interfaces/superfluid/ISuperfluid.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import {
    GeneralDistributionAgreementV1
} from "../agreements/GeneralDistributionAgreementV1.sol";
import {
    ISuperTokenPool,
    SuperTokenPool
} from "../superfluid/SuperTokenPool.sol";


/// @title GeneralDistributionAgreementV1Mock
/// @author Superfluid
/// @notice A GDAv1 Mock contract which exposes internal functions for testing purposes
contract GeneralDistributionAgreementV1Mock is GeneralDistributionAgreementV1 {
    constructor(ISuperfluid host) GeneralDistributionAgreementV1(host) {}

    function setUIndex(
        bytes memory eff,
        address owner,
        BasicParticle memory p
    ) public returns (bytes memory) {
        return _setUIndex(eff, owner, p);
    }

    function getUIndexData(
        bytes memory eff,
        address owner
    ) public view returns (UniversalIndexData memory universalIndexData) {
        return _getUIndexData(eff, owner);
    }

    function getUIndex(
        bytes memory eff,
        address owner
    ) public view returns (BasicParticle memory uIndex) {
        return _getUIndex(eff, owner);
    }

    function setPDPIndex(
        bytes memory eff,
        address owner,
        PDPoolIndex memory pdpIndex
    ) public returns (bytes memory) {
        return _setPDPIndex(eff, owner, pdpIndex);
    }

    function getPDPIndex(
        bytes memory eff,
        address owner
    ) public view returns (PDPoolIndex memory pdpIndex) {
        return _getPDPIndex(eff, owner);
    }

    function adjustBuffer(
        bytes memory eff,
        address from,
        bytes32 flowHash,
        FlowRate oldFlowRate,
        FlowRate newFlowRate
    ) public returns (bytes memory) {
        return _adjustBuffer(eff, from, flowHash, oldFlowRate, newFlowRate);
    }

    function setFlowInfo(
        bytes memory eff,
        bytes32 flowHash,
        address from,
        address to,
        FlowRate newFlowRate,
        FlowRate flowRateDelta
    ) public returns (bytes memory) {
        return
            _setFlowInfo(eff, flowHash, from, to, newFlowRate, flowRateDelta);
    }

    function getFlowDistributionData(
        ISuperfluidToken token,
        bytes32 distributionFlowId
    )
        public
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        return _getFlowDistributionData(token, distributionFlowId);
    }

    function getFlowRate(
        bytes memory eff,
        bytes32 distributionFlowId
    ) public view returns (FlowRate flowRate) {
        return _getFlowRate(eff, distributionFlowId);
    }

    function isPool(
        ISuperfluidToken token,
        address account
    ) public view returns (bool) {
        return _isPool(token, account);
    }

    function getFlowDistributionId(
        address from,
        address to
    ) public view returns (bytes32) {
        return _getFlowDistributionId(from, to);
    }

    function getPoolMemberId(
        address poolMember,
        ISuperTokenPool pool
    ) public view returns (bytes32) {
        return _getPoolMemberId(poolMember, pool);
    }

    function getPoolMemberData(
        ISuperfluidToken token,
        address poolMember,
        ISuperTokenPool pool
    ) public view returns (bool exist, PoolMemberData memory poolMemberData) {
        return _getPoolMemberData(token, poolMember, pool);
    }

    function encodePoolMemberData(
        PoolMemberData memory poolMemberData
    ) public pure returns (bytes32[] memory) {
        return _encodePoolMemberData(poolMemberData);
    }
}
