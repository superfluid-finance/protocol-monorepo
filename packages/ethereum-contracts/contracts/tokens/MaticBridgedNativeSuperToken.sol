// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperToken,
    CustomSuperTokenBase
}
from "../interfaces/superfluid/CustomSuperTokenBase.sol";
import { IMaticBridgedNativeSuperTokenCustom } from "../interfaces/tokens/IMaticBridgedNativeSuperToken.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";


/**
 * @dev Native SuperToken with interfaces for the Matic POS bridge to mint and burn.
 * See https://docs.polygon.technology/docs/develop/ethereum-matic/pos/mapping-assets/
 *
 * @author Superfluid
 */
contract MaticBridgedNativeSuperTokenProxy is IMaticBridgedNativeSuperTokenCustom, CustomSuperTokenBase, UUPSProxy {
    address public childChainManager;

    constructor(address childChainManager_) {
        childChainManager = childChainManager_;
    }

    function deposit(address user, bytes calldata depositData) external override {
        require(msg.sender == childChainManager, "MBNSuperToken: no permission to deposit");
        uint256 amount = abi.decode(depositData, (uint256));
        ISuperToken(address(this)).selfMint(user, amount, new bytes(0));
    }

    function withdraw(uint256 amount) external override {
        ISuperToken(address(this)).selfBurn(msg.sender, amount, new bytes(0));
    }

    function updateChildChainManager(address newChildChainManager) external override {
        address host = ISuperToken(address(this)).getHost();
        address gov = address(ISuperfluid(host).getGovernance());
        require(msg.sender == gov, "MBNSuperToken: only governance allowed");

        childChainManager = newChildChainManager;
        emit ChildChainManagerChanged(newChildChainManager);
    }
}
