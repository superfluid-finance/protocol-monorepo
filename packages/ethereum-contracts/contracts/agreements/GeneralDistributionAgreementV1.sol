// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { EnumerableSet } from "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance,
    ISuperApp,
    SuperAppDefinitions,
    ContextDefinitions,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import {
    BasicParticle,
    SemanticMoney,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
    FlowRate,
    Time,
    Unit,
    Value
} from "../libs/SemanticMoney.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";
import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";
import { ISuperTokenPool } from "../interfaces/superfluid/ISuperTokenPool.sol";
import { AgreementBase } from "./AgreementBase.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";

/**
 * @title General Distribution Agreement
 * @author Superfluid
 * @notice
 */
contract GeneralDistributionAgreementV1 is
    AgreementBase,
    IGeneralDistributionAgreementV1
{
    using EnumerableSet for EnumerableSet.AddressSet;

    mapping (address owner => EnumerableSet.AddressSet) internal _connectionsMap;

    constructor(ISuperfluid host) AgreementBase(address(host)) {}

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperfluidToken token,
        address account,
        uint256 time
    )
        external
        view
        override
        returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
    {
    }

    function createPool() external returns (SuperTokenPool pool) {
        pool = new SuperTokenPool(msg.sender);
    }

    // @note This is same as approveSubscription
    function connectPool(SuperTokenPool pool) external returns (bool success) {

    }

    // @note This is same as revokeSubscription
    function disconnectPool(SuperTokenPool pool) external returns (bool success) {
        // disconnect via slotsbitmap
    }

    function connectPool(SuperTokenPool pool, bool doConnect) public returns (bool success) {
        if (doConnect) {
            if (!_connectionsMap[msg.sender].contains(address(pool))) {
                _connectionsMap[msg.sender].add(address(pool));
                assert(pool.operatorConnectMember(uint32(block.timestamp), msg.sender, true));
            }
        } else {
            if (_connectionsMap[msg.sender].contains(address(pool))) {
                _connectionsMap[msg.sender].remove(address(pool));
                assert(pool.operatorConnectMember(uint32(block.timestamp), msg.sender, false));
            }
        }
        success = true;
    }

    function isMemberConnected(address pool, address member) external view returns (bool) {
        return _connectionsMap[member].contains(pool);
    }

    function distribute(
        ISuperfluidToken token,
        ISuperTokenPool pool,
        uint256 amount,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);
        newCtx = ctx;
    }

    function distributeFlow(
        ISuperfluidToken token,
        ISuperTokenPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) {
        ISuperfluid.Context memory currentContext = AgreementLibrary
            .authorizeTokenAccess(token, ctx);

        newCtx = ctx;
    }
}
