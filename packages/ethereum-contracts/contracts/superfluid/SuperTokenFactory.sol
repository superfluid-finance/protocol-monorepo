// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    ISuperTokenFactory,
    ISuperToken,
    IERC20,
    ERC20WithTokenInfo,
    SuperfluidErrors
} from "../interfaces/superfluid/ISuperTokenFactory.sol";

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import { SuperToken } from "../superfluid/SuperToken.sol";

import { FullUpgradableSuperTokenProxy } from "./FullUpgradableSuperTokenProxy.sol";

abstract contract SuperTokenFactoryBase is
    UUPSProxiable,
    ISuperTokenFactory
{

    ISuperfluid immutable internal _host;

    ISuperToken internal _superTokenLogic;

    /// @notice A mapping from underlying token addresses to canonical wrapper super token addresses
    /// @dev Reasoning: (1) provide backwards compatibility for existing listed wrapper super tokens
    /// @dev (2) prevent address retrieval issues if we ever choose to modify the bytecode of the UUPSProxy contract
    mapping(address => address) internal _canonicalWrapperSuperTokens;

    constructor(
        ISuperfluid host
    ) {
        _host = host;
    }

    /// @inheritdoc ISuperTokenFactory
    function getHost()
       external view
       override(ISuperTokenFactory)
       returns(address host)
    {
       return address(_host);
    }

    /**************************************************************************
    * UUPSProxiable
    **************************************************************************/
    /// @inheritdoc ISuperTokenFactory
    function initialize()
        external override
        initializer // OpenZeppelin Initializable
    {
        _updateSuperTokenLogic();
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperTokenFactory.implementation");
    }

    function updateCode(address newAddress) external override {
        if (msg.sender != address(_host)) {
            revert SuperfluidErrors.ONLY_HOST(SuperfluidErrors.SUPER_TOKEN_FACTORY_ONLY_HOST);
        }
        _updateCodeAddress(newAddress);
        _updateSuperTokenLogic();
    }

    function _updateSuperTokenLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _superTokenLogic = SuperToken(this.createSuperTokenLogic(_host));
        UUPSProxiable(address(_superTokenLogic)).castrate();
        emit SuperTokenLogicCreated(_superTokenLogic);
    }

    /**************************************************************************
    * ISuperTokenFactory
    **************************************************************************/
    /// @inheritdoc ISuperTokenFactory
    function getSuperTokenLogic()
        external view override
        returns (ISuperToken)
    {
        return _superTokenLogic;
    }

    function createSuperTokenLogic(ISuperfluid host) external virtual returns (address logic);

    /// @inheritdoc ISuperTokenFactory
    function createCanonicalERC20Wrapper(ERC20WithTokenInfo _underlyingToken)
        external
        returns (ISuperToken)
    {
        address underlyingTokenAddress = address(_underlyingToken);
        address canonicalSuperTokenAddress = _canonicalWrapperSuperTokens[
                underlyingTokenAddress
            ];

        // if the canonical super token address exists, just return
        if (canonicalSuperTokenAddress != address(0)) {
            return ISuperToken(canonicalSuperTokenAddress);
        }

        // use create2 to deterministically create the proxy contract for the wrapper super token
        bytes32 salt = keccak256(abi.encode(underlyingTokenAddress));
        UUPSProxy proxy = new UUPSProxy{ salt: salt }();

        // NOTE: address(proxy) is equivalent to address(superToken)
        _canonicalWrapperSuperTokens[underlyingTokenAddress] = address(
            proxy
        );

        // set the implementation/logic contract address for the newly deployed proxy
        proxy.initializeProxy(address(_superTokenLogic));

        // cast it as the same type as the logic contract
        ISuperToken superToken = ISuperToken(address(proxy));

        // get underlying token info
        uint8 underlyingDecimals = _underlyingToken.decimals();
        string memory underlyingName = _underlyingToken.name();
        string memory underlyingSymbol = _underlyingToken.symbol();
        // initialize the contract (proxy constructor)
        superToken.initialize(
            _underlyingToken,
            underlyingDecimals,
            string.concat("Super ", underlyingName),
            string.concat(underlyingSymbol, "x")
        );

        emit SuperTokenCreated(superToken);

        return superToken;
    }

    /// @inheritdoc ISuperTokenFactory
    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        public override
        returns (ISuperToken superToken)
    {
        if (address(underlyingToken) == address(0)) {
            revert SuperfluidErrors.ZERO_ADDRESS(SuperfluidErrors.SUPER_TOKEN_FACTORY_ZERO_ADDRESS);
        }

        if (upgradability == Upgradability.NON_UPGRADABLE) {
            superToken = ISuperToken(this.createSuperTokenLogic(_host));
        } else if (upgradability == Upgradability.SEMI_UPGRADABLE) {
            UUPSProxy proxy = new UUPSProxy();
            // initialize the wrapper
            proxy.initializeProxy(address(_superTokenLogic));
            superToken = ISuperToken(address(proxy));
        } else /* if (type == Upgradability.FULL_UPGRADABE) */ {
            FullUpgradableSuperTokenProxy proxy = new FullUpgradableSuperTokenProxy();
            proxy.initialize();
            superToken = ISuperToken(address(proxy));
        }

        // initialize the token
        superToken.initialize(
            underlyingToken,
            underlyingDecimals,
            name,
            symbol
        );

        emit SuperTokenCreated(superToken);
    }

    /// @inheritdoc ISuperTokenFactory
    function createERC20Wrapper(
        ERC20WithTokenInfo underlyingToken,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external override
        returns (ISuperToken superToken)
    {
        return createERC20Wrapper(
            underlyingToken,
            underlyingToken.decimals(),
            upgradability,
            name,
            symbol
        );
    }

    /// @inheritdoc ISuperTokenFactory
    function initializeCustomSuperToken(
        address customSuperTokenProxy
    )
        external override
    {
        // odd solidity stuff..
        // NOTE payable necessary because UUPSProxy has a payable fallback function
        address payable a = payable(address(uint160(customSuperTokenProxy)));
        UUPSProxy(a).initializeProxy(address(_superTokenLogic));

        emit CustomSuperTokenCreated(ISuperToken(customSuperTokenProxy));
    }

    /// @inheritdoc ISuperTokenFactory
    function computeWrapperSuperTokenAddress(address _underlyingToken)
        external
        view
        returns (address superTokenAddress, bool isDeployedAndCanonical)
    {
        address existingAddress = _canonicalWrapperSuperTokens[
            _underlyingToken
        ];

        if (existingAddress != address(0)) {
            superTokenAddress = existingAddress;
            isDeployedAndCanonical = true;
        } else {
            bytes memory bytecode = type(UUPSProxy).creationCode;
            superTokenAddress = address(
                uint160(
                    uint256(
                        keccak256(
                            abi.encodePacked(
                                bytes1(0xff),
                                address(this),
                                keccak256(abi.encode(_underlyingToken)),
                                keccak256(bytecode)
                            )
                        )
                    )
                )
            );
            isDeployedAndCanonical = false;
        }
    }
}

// splitting this off because the contract is getting bigger
contract SuperTokenFactoryHelper {
    function create(ISuperfluid host)
        external
        returns (address logic)
    {
        return address(new SuperToken(host));
    }
}

contract SuperTokenFactory is SuperTokenFactoryBase
{
    SuperTokenFactoryHelper immutable private _helper;

    constructor(
        ISuperfluid host,
        SuperTokenFactoryHelper helper
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
        _helper = helper;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return _helper.create(host);
    }
}
