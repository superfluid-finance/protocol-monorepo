// SPDX-License-Identifier: UNLICENSED
pragma solidity >= 0.8.4;

import {
    ISuperfluidPool, ISuperfluidPoolAdmin
} from "./ISuperfluidPool.sol";
import {
    Time, FlowRate, Value, Unit,
    BasicParticle
} from "../SemanticMoney.sol";


type FlowId is uint32;

/**
 * @dev The interface for super token: the ERC20x token that supports generalized payment primitives.
 */
interface ISuperfluidToken is ISuperfluidPoolAdmin {
    ////////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    ////////////////////////////////////////////////////////////////////////////////

    function realtimeBalanceOf(address account) external view returns (Value rtb);

    function realtimeBalanceAt(address account, Time t) external view returns (Value rtb);

    function realtimeBalanceVectorAt(address account, Time t) external view returns (Value available, Value deposit);

    function getNetFlowRate(address account) external view returns (FlowRate);

    function getFlowRate(address from, address to, FlowId flowId) external view returns (FlowRate);

    // REVIEW NOTES:
    // - flowAddress naming concern: uniquely obtained AppId? flowId (renaming input to flowSubId)?
    // - nFlows/totalFlowRate between sender and receiver is lost.
    // - in v1 separation of outgoing/incoming is lost.

    function shift(address from, address to, Value amount) external
        returns (bool);

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) external
        returns (bool);

    function distribute(address from, ISuperfluidPool to, Value reqAmount) external
        returns (bool, Value actualAmount);

    function distributeFlow(address from, ISuperfluidPool to, FlowId flowId, FlowRate reqFlowRate) external
        returns (bool success, FlowRate actualFlowRate, FlowRate newDistributionFlowRate);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    // REVIEW NOTES:
    // - connectPool has implied side effects and claimAll.

    function isPool(address p) external view
        returns (bool);

    function getNumConnections(address account) external view
        returns (uint);

    function connectPool(ISuperfluidPool to) external
        returns (bool);

    function disconnectPool(ISuperfluidPool to) external
        returns (bool);

    function connectPool(ISuperfluidPool to, bool doConnect) external
        returns (bool);
}
