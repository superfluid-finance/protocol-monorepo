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

/**
 * @dev Native SuperToken custom super token implementation
 * Logic of native token overlay upgradable independent of Supertoken Logic
 *
 * How to deploy:
 * auxLogic = AuxNativeSuperTokenLogic.new()
 * proxy = UpgradableNativeSuperTokenProxy.new(auxLogic)
 * proxy.initializeProxy(superTokenLogic) // usually done by SuperTokenFactory
 *
 * @author Superfluid
 */
contract UpgradableNativeSuperTokenProxy is INativeSuperTokenCustom, CustomSuperTokenBase, Ownable, UUPSProxy {
    AuxNativeSuperTokenLogic internal _auxLogic;

    constructor(AuxNativeSuperTokenLogic auxLogic) {
        _auxLogic = auxLogic;
    }

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

contract AuxNativeSuperTokenLogic is CustomSuperTokenBase {
    address[2] internal _proxyPadding;
    uint256 internal _secret;

    function implementsFn(bytes4 selector) external pure returns(bool) {
        return
            selector == this.initializeAux.selector ||
            selector == this.randomFun.selector ||
            selector == this.getSecret.selector
        ;
    }

    function initializeAux(uint256 secret) public {
        _secret = secret;
    }

    event RandomEvent(address sender);
    function randomFun() public {
        emit RandomEvent(msg.sender);
    }

    function getSecret() external view returns(uint256) {
        return _secret;
    }
}
