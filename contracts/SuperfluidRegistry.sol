// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;

import "@openzeppelin/contracts/utils/Create2.sol";
import "@openzeppelin/contracts/utils/Address.sol";
import "./interface/ISuperfluidRegistry.sol";
import "./interface/ISuperfluidGovernance.sol";
import "./SuperToken.sol";
import "./Proxy.sol";

contract SuperfluidRegistry is ISuperfluidRegistry {

    ISuperfluidGovernance private _gov;
    SuperToken private _superTokenLogic;

    constructor(ISuperfluidGovernance gov) public {
        _gov = gov;
        _superTokenLogic = new SuperToken();
    }

    function getERC20Wrapper(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    override
    returns (address wrapperAddress, bool created) {
        bytes32 salt = _genereateERC20WrapperSalt(name, symbol, decimals, token);
        wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        created = Address.isContract(wrapperAddress);
    }

    function createERC20Wrapper(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    override
    returns (ISuperToken)
    {
        require(address(token) != address(0), "SuperfluidRegistry: ZERO_ADDRESS");
        bytes32 salt = _genereateERC20WrapperSalt(name, symbol, decimals, token);
        address wrapperAddress = Create2.computeAddress(salt, keccak256(type(Proxy).creationCode));
        require(!Address.isContract(wrapperAddress), "SuperfluidRegistry: WRAPPER_EXIST");
        Proxy proxy = new Proxy{salt: salt}();
        proxy.initializeProxy(address(_superTokenLogic));
        require(wrapperAddress == address(proxy), "Superfluid: UNEXPECTED_WRAPPER_ADDRESS");
        // initialize the token
        SuperToken superToken = SuperToken(address(proxy));
        superToken.initialize(
            name,
            symbol,
            decimals,
            token,
            _gov
        );
    }

    function getGovernance()
    external
    override
    returns (ISuperfluidGovernance)
    {
        return _gov;
    }

    function _genereateERC20WrapperSalt(
        string memory name,
        string memory symbol,
        uint8 decimals,
        IERC20 token
    ) private pure returns (bytes32 salt) {
        return keccak256(abi.encodePacked(
            name,
            symbol,
            decimals,
            token
        ));
    }
}
