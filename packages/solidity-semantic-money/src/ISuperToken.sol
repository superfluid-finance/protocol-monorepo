// SPDX-License-Identifier: UNLICENSED
pragma solidity >= 0.8.4;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import "@superfluid-finance/solidity-semantic-money/src/ISuperTokenPool.sol";


type FlowId is uint32;

/**
 * @dev The interface for super token: the ERC20x token that supports generalized payment primitives.
 */
interface ISuperToken is IERC20 {
    ////////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    ////////////////////////////////////////////////////////////////////////////////

    //function realtimeBalanceOf(address account) returns (int256 avb, int256 deposit, int256 ...) {
    //}

    function iTransfer(address from, address to, Value amount) external
        returns (bool);

    function flow(address from, address to, FlowId flowId, FlowRate flowRate) external
        returns (bool);

    function distribute(address from, ISuperTokenPool to, Value reqAmount) external
        returns (bool, Value actualAmount);

    function distributeFlow(address from, ISuperTokenPool to, FlowId flowId, FlowRate reqFlowRate) external
        returns (bool success, FlowRate actualFlowRate);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    function connectPool(ISuperTokenPool to) external
        returns (bool);

    function disconnectPool(ISuperTokenPool to) external
        returns (bool);

    function connectPool(ISuperTokenPool to, bool doConnect) external
        returns (bool);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Owner Operations
    ////////////////////////////////////////////////////////////////////////////////

    function isMemberConnected(ISuperTokenPool pool, address memberAddr) external view
        returns (bool);

    /// This is used by the pool to adjust flow rate
    function absorbParticleFromPool(address account, BasicParticle calldata p) external
        returns (bool);
}
