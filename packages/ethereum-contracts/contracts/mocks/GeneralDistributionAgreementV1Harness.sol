// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { GeneralDistributionAgreementV1 } from "../agreements/GeneralDistributionAgreementV1.sol";
import { ISuperfluidPool, SuperfluidPool } from "../superfluid/SuperfluidPool.sol";

/// @title GeneralDistributionAgreementV1Harness
/// @author Superfluid
/// @notice A GDAv1 Mock contract which exposes internal functions for testing purposes
contract GeneralDistributionAgreementV1Harness is GeneralDistributionAgreementV1 {
    constructor(ISuperfluid host) GeneralDistributionAgreementV1(host) { }

    function setUIndex(bytes memory eff, address owner, BasicParticle memory p) external returns (bytes memory) {
        return _setUIndex(eff, owner, p);
    }

    function getUIndexAndUindexData(bytes memory eff, address owner)
        external
        view
        returns (BasicParticle memory uIndex, UniversalIndexData memory universalIndexData)
    {
        uIndex = _getUIndex(eff, owner);
        universalIndexData = _getUIndexData(eff, owner);
    }

    function setAndGetPDPIndex(bytes memory eff, address owner, PDPoolIndex memory pdpIndex)
        external
        returns (bytes memory, PDPoolIndex memory returnedPdpIndex)
    {
        return (_setPDPIndex(eff, owner, pdpIndex), _getPDPIndex(eff, owner));
    }

    function adjustBuffer(bytes memory eff, address pool, address from, FlowRate oldFlowRate, FlowRate newFlowRate)
        external
        returns (bytes memory)
    {
        bytes32 flowHash = _getFlowDistributionHash(from, ISuperfluidPool(pool));
        return _adjustBuffer(eff, pool, from, flowHash, oldFlowRate, newFlowRate);
    }

    function setFlowInfo(bytes memory eff, address from, address to, FlowRate newFlowRate, FlowRate flowRateDelta)
        external
        returns (bytes memory)
    {
        bytes32 flowHash = _getFlowDistributionHash(from, ISuperfluidPool(to));
        return _setFlowInfo(eff, flowHash, from, to, newFlowRate, flowRateDelta);
    }

    function getFlowDistributionData(ISuperfluidToken token, address from, address to)
        external
        view
        returns (bool exist, FlowDistributionData memory flowDistributionData)
    {
        bytes32 distributionFlowId = _getFlowDistributionHash(from, ISuperfluidPool(to));
        return _getFlowDistributionData(token, distributionFlowId);
    }

    function getFlowRate(bytes memory eff, address from, address to) external view returns (FlowRate flowRate) {
        bytes32 distributionFlowId = _getFlowDistributionHash(from, ISuperfluidPool(to));
        return _getFlowRate(eff, distributionFlowId);
    }

    function getPoolMemberId(address poolMember, ISuperfluidPool pool) external view returns (bytes32) {
        return _getPoolMemberHash(poolMember, pool);
    }

    function getPoolMemberData(ISuperfluidToken token, address poolMember, ISuperfluidPool pool)
        external
        view
        returns (bool exist, PoolMemberData memory poolMemberData)
    {
        return _getPoolMemberData(token, poolMember, pool);
    }

    function encodePoolMemberData(PoolMemberData memory poolMemberData) external pure returns (bytes32[] memory) {
        return _encodePoolMemberData(poolMemberData);
    }
}
