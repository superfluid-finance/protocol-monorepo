// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperToken,
    CustomSuperTokenBase
}
from "../interfaces/superfluid/CustomSuperTokenBase.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

/**
 * @dev Native SuperToken with (minting) interfaces for the Matic POS bridge
 * See https://docs.polygon.technology/docs/develop/ethereum-matic/pos/mapping-assets/
 *
 * @author Superfluid
 */
contract MaticBridgedNativeSuperTokenProxy is CustomSuperTokenBase, UUPSProxy {
    address public childChainManagerProxy;
    // TODO replace with existing privileged account
    address deployer;

    constructor(address _childChainManagerProxy) {
        childChainManagerProxy = _childChainManagerProxy;
        deployer = msg.sender;
    }

    // being proxified smart contract, most probably childChainManagerProxy contract's address
    // is not going to change ever, but still, lets keep it
    function updateChildChainManager(address newChildChainManagerProxy) external {
        require(newChildChainManagerProxy != address(0), "Bad ChildChainManagerProxy address");
        require(msg.sender == deployer, "You're not allowed");

        childChainManagerProxy = newChildChainManagerProxy;
    }

    function deposit(address user, bytes calldata depositData) external {
        require(msg.sender == childChainManagerProxy, "You're not allowed to deposit");
        uint256 amount = abi.decode(depositData, (uint256));
        ISuperToken(address(this)).selfMint(user, amount, new bytes(0));
    }

    function withdraw(uint256 amount) external {
        ISuperToken(address(this)).selfBurn(msg.sender, amount, new bytes(0));
    }
}
