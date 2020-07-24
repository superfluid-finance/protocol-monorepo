// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;

import "@openzeppelin/contracts/utils/Create2.sol";
import "@openzeppelin/contracts/utils/Address.sol";
import "./upgradability/Proxy.sol";
import "./upgradability/Proxiable.sol";
import "./interface/Ownable.sol";
import "./interface/ISuperfluidRegistry.sol";
import "./interface/ISuperfluidGovernance.sol";
import "./SuperToken.sol";

contract SuperfluidRegistryStorage {
    /* WARNING: NEVER RE-ORDER VARIABLES! Always double-check that new
       variables are added APPEND-ONLY. Re-ordering variables can
       permanently BREAK the deployed proxy contract. */

    /// @dev Flag to avoid double initialization
    bool internal _initialized;

    /// @dev Governance contract
    ISuperfluidGovernance internal _gov;

    /// @dev Super token logic contract
    ISuperToken internal _superTokenLogic;
}

contract SuperfluidRegistry is
    Ownable,
    SuperfluidRegistryStorage,
    ISuperfluidRegistry,
    Proxiable {

    function initialize() external {
        require(!_initialized, "The library has already been initialized.");
        _owner = msg.sender;
        _initialized = true;
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

    function getGovernance() external override returns (ISuperfluidGovernance) { return _gov; }
    function setGovernance(ISuperfluidGovernance gov) external onlyOwner { _gov = gov; }
    function getSuperTokenLogic() external override returns (ISuperToken) { return _superTokenLogic; }
    function setSuperTokenLogic(ISuperToken logic) external onlyOwner { _superTokenLogic = logic; }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperfluidRegistry.implementation");
    }

    function updateCode(address newAddress) external onlyOwner {
        return _updateCodeAddress(newAddress);
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
