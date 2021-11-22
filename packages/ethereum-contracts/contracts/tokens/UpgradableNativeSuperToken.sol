// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperToken,
    CustomSuperTokenBase
}
from "../interfaces/superfluid/CustomSuperTokenBase.sol";
import { INativeSuperTokenCustom } from "../interfaces/tokens/INativeSuperToken.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";

// minimal interface for an auxiliary logic contract
interface IAuxLogic {
    // returns true if calls matching the given selector shall be delegated ot the auxiliary logic contract
    function implementsFn(bytes4 selector) external pure returns(bool);
}

/**
 * @dev PoC of a custom native super token implementation which is fully upgradable.
 * Additionally to using the UUPSProxy pattern for delegating to upgradable SuperToken logic,
 * this contract can have additional custom functionality implemented by an auxiliary logic contract.
 * This "auxLogic" contract is upgradable by the proxy contract owner, independent of the SuperToken logic admin.
 *
 * @author Superfluid
 */
contract UpgradableNativeSuperTokenProxy is INativeSuperTokenCustom, CustomSuperTokenBase, Ownable, UUPSProxy {
    IAuxLogic internal _auxLogic;

    // The first version of the auxiliary logic can already be defined when deploying.
    constructor(IAuxLogic auxLogic) {
        _auxLogic = auxLogic;
    }

    // Allows upgrading to a new version of auxLogic.
    // The caller is fully responsible for keeping compatibility with the previous storage structure when upgrading.
    // An incompatible new logic contract can brick the whole contract, e.g. by omitting storage paddings.
    event AuxLogicUpgraded(IAuxLogic newAuxLogix);
    function upgradeAuxLogic(IAuxLogic newAuxLogic) external onlyOwner {
        _auxLogic = newAuxLogic;
        emit AuxLogicUpgraded(newAuxLogic);
    }

    // Initialization of the "conventional" logic (UUPSProxy pattern)
    function initialize(string calldata name, string calldata symbol, uint256 initialSupply)
        external override
    {
        ISuperToken(address(this)).initialize(
            IERC20(0x0), // no underlying/wrapped token
            18, // shouldn't matter if there's no wrapped token
            name,
            symbol
        );
        ISuperToken(address(this)).selfMint(msg.sender, initialSupply, new bytes(0));
    }

    // Dispatcher for all other calls
    // Checks if the auxLogic wants to handle before delegating to the UUPSProxy implementation.
    function _fallback() internal virtual override {
        _beforeFallback();

        // check if it targets the auxLogic
        if (_auxLogic.implementsFn(msg.sig)) {
            _delegate(address(_auxLogic));
        } else {
            _delegate(_implementation());
        }
    }
}

// v1 of a possible implementation of auxiliary logic.
// Inherits from CustomSuperTokenBase in order to prepend storage paddings as required by the SuperToken implementation.
contract AuxNativeSuperTokenLogicV1 is IAuxLogic, CustomSuperTokenBase {
    // Additional paddings to avoid messing with the storage managed by the Proxy contract itself.
    // slot1 is occupied by Ownable._owner, slot2 for storing the auxLogic contract address.
    address[2] internal _proxyPadding;

    // type and order of variables need to be preserved in future versions!

    // this initialization has no effect when used in delegate calls from the Proxy,
    // but prevents the logic contract itself being initialized (by accident or not)
    bool internal _initialized = true;
    uint256 internal _theAnswer;

    // Allows the proxy to check if this contract wants to handle a given call.
    // All functions supposed to be reachable via proxy need to be included here.
    function implementsFn(bytes4 selector) external override pure returns(bool) {
        return
            selector == this.initializeAux.selector ||
            selector == this.randomFun.selector ||
            selector == this.getSecret.selector
        ;
    }

    // ==================================
    // Additional logic for demo purposes
    // ==================================

    // If there's any state variables to be initialized, that needs to be done with an initializer function.
    // Neither direct variable initialization nor initialization in a constructor would have any effect.
    // Note that this example is without permissioning
    function initializeAux(uint256 theAnswer) public {
        require(!_initialized, "already initialized");
        _theAnswer = theAnswer;
        _initialized = true;
    }

    event RandomEvent(address sender);
    function randomFun() public {
        emit RandomEvent(msg.sender);
    }

    function getSecret() external view returns(uint256) {
        return _theAnswer;
    }
}
