// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    ISuperTokenFactory,
    ISuperToken,
    IERC20,
    ERC20WithTokenInfo
} from "../interfaces/superfluid/ISuperTokenFactory.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantOutflowNFT } from "../interfaces/superfluid/IConstantOutflowNFT.sol";
import { IConstantInflowNFT } from "../interfaces/superfluid/IConstantInflowNFT.sol";
import { IPoolAdminNFT } from "../interfaces/superfluid/IPoolAdminNFT.sol";
import { IPoolMemberNFT } from "../interfaces/superfluid/IPoolMemberNFT.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import { FullUpgradableSuperTokenProxy } from "./FullUpgradableSuperTokenProxy.sol";
import { ConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";
import { SuperfluidNFTDeployerLibrary } from "../libs/SuperfluidNFTDeployerLibrary.sol";

// @note TODO must deploy and link the SuperfluidNFTDeployerLibrary contract in deploy-framework.js

abstract contract SuperTokenFactoryBase is
    UUPSProxiable,
    ISuperTokenFactory
{
    struct InitializeData {
        address underlyingToken;
        address superToken;
    }

    /* WARNING: NEVER RE-ORDER VARIABLES! Including the base contracts.
        Always double-check that new
        variables are added APPEND-ONLY. Re-ordering variables can
        permanently BREAK the deployed proxy contract. */

    ISuperfluid immutable internal _host;

    ISuperToken internal _superTokenLogic;

    /// @notice A mapping from underlying token addresses to canonical wrapper super token addresses
    /// @dev Reasoning: (1) provide backwards compatibility for existing listed wrapper super tokens
    /// @dev (2) prevent address retrieval issues if we ever choose to modify the bytecode of the UUPSProxy contract
    /// @dev NOTE: address(0) key points to the NativeAssetSuperToken on the network.
    mapping(address => address) internal _canonicalWrapperSuperTokens;

    IConstantOutflowNFT internal _constantOutflowNFTLogic;
    IConstantInflowNFT internal _constantInflowNFTLogic;

    IPoolAdminNFT internal _poolAdminNFTLogic;
    IPoolMemberNFT internal _poolMemberNFTLogic;
    
    /// NOTE: Whenever modifying the storage layout here it is important to update the validateStorageLayout
    /// function in its respective mock contract to ensure that it doesn't break anything or lead to unexpected
    /// behaviors/layout when upgrading

    error SUPER_TOKEN_FACTORY_ONLY_GOVERNANCE_OWNER();

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

    /// @notice Updates the logic contract for the SuperTokenFactory
    /// @dev This function updates the logic contract for the SuperTokenFactory
    /// It also updates the logic contract for the SuperToken and the respective NFTs:
    /// ConstantOutflowNFT, ConstantInflowNFT, PoolAdminNFT, PoolMemberNFT
    /// @param newAddress the new address of the SuperTokenFactory logic contract
    function updateCode(address newAddress) external override {
        if (msg.sender != address(_host)) {
            revert SUPER_TOKEN_FACTORY_ONLY_HOST();
        }

        // point at the new logic contract for the SuperTokenFactory
        _updateCodeAddress(newAddress);

        _updateSuperTokenLogic();
        _updateConstantOutflowNFTLogic();
        _updateConstantInflowNFTLogic();
    }

    function _updateSuperTokenLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _superTokenLogic = SuperToken(this.createSuperTokenLogic(_host));
        UUPSProxiable(address(_superTokenLogic)).castrate();
        emit SuperTokenLogicCreated(_superTokenLogic);
    }

    function _updateConstantOutflowNFTLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _constantOutflowNFTLogic = IConstantOutflowNFT(this.createConstantOutflowNFTLogic());
        UUPSProxiable(address(_constantOutflowNFTLogic)).castrate();
        emit ConstantOutflowNFTLogicCreated(_constantOutflowNFTLogic);
    }

    function _updateConstantInflowNFTLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _constantInflowNFTLogic = IConstantInflowNFT(this.createConstantInflowNFTLogic());
        UUPSProxiable(address(_constantInflowNFTLogic)).castrate();
        emit ConstantInflowNFTLogicCreated(_constantInflowNFTLogic);
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

    function createConstantOutflowNFTLogic() external virtual returns (address logic) {
        return SuperfluidNFTDeployerLibrary.deployConstantOutflowNFT();
    }
    function createConstantInflowNFTLogic() external virtual returns (address logic) {
        return SuperfluidNFTDeployerLibrary.deployConstantInflowNFT();
    }

    /// @inheritdoc ISuperTokenFactory
    function createCanonicalERC20Wrapper(ERC20WithTokenInfo _underlyingToken)
        external
        returns (ISuperToken)
    {
        // we use this to check if we have initialized the _canonicalWrapperSuperTokens mapping
        // @note we must set this during initialization
        if (_canonicalWrapperSuperTokens[address(0)] == address(0)) {
            revert SUPER_TOKEN_FACTORY_UNINITIALIZED();
        }

        address underlyingTokenAddress = address(_underlyingToken);
        address canonicalSuperTokenAddress = _canonicalWrapperSuperTokens[
                underlyingTokenAddress
            ];

        // if the canonical super token address exists, revert with custom error
        if (canonicalSuperTokenAddress != address(0)) {
            revert SUPER_TOKEN_FACTORY_ALREADY_EXISTS();
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
            revert SUPER_TOKEN_FACTORY_ZERO_ADDRESS();
        }

        if (upgradability == Upgradability.NON_UPGRADABLE) {
            superToken = ISuperToken(this.createSuperTokenLogic(_host));
        } else if (upgradability == Upgradability.SEMI_UPGRADABLE) {
            UUPSProxy proxy = new UUPSProxy();
            // initialize the wrapper
            proxy.initializeProxy(address(_superTokenLogic));
            superToken = ISuperToken(address(proxy));
        } else /* if (type == Upgradability.FULL_UPGRADABLE) */ {
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
    function computeCanonicalERC20WrapperAddress(address _underlyingToken)
        external
        view
        returns (address superTokenAddress, bool isDeployed)
    {
        address existingAddress = _canonicalWrapperSuperTokens[
            _underlyingToken
        ];

        if (existingAddress != address(0)) {
            superTokenAddress = existingAddress;
            isDeployed = true;
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
            isDeployed = false;
        }
    }

    /// @inheritdoc ISuperTokenFactory
    function getCanonicalERC20Wrapper(address _underlyingTokenAddress)
        external
        view
        returns (address superTokenAddress)
    {
        superTokenAddress = _canonicalWrapperSuperTokens[
            _underlyingTokenAddress
        ];
    }

    /// @notice Initializes list of canonical wrapper super tokens.
    /// @dev Note that this should also be kind of a throwaway function which will be executed only once.
    /// @param _data an array of canonical wrappper super tokens to be set
    function initializeCanonicalWrapperSuperTokens(
        InitializeData[] calldata _data
    ) external virtual  {
        Ownable gov = Ownable(address(_host.getGovernance()));
        if (msg.sender != gov.owner()) revert SUPER_TOKEN_FACTORY_ONLY_GOVERNANCE_OWNER();

        // once the list has been set, it cannot be reset
        // @note this means that we must set the 0 address (Native Asset Super Token) when we call this the first time
        if (_canonicalWrapperSuperTokens[address(0)] != address(0)) {
            revert SUPER_TOKEN_FACTORY_ALREADY_EXISTS();
        }

        // initialize mapping
        for (uint256 i = 0; i < _data.length; i++) {
            _canonicalWrapperSuperTokens[_data[i].underlyingToken] = _data[i]
                .superToken;
        }
    }

    /// @inheritdoc ISuperTokenFactory
    function deployNFTProxyContractsAndInititialize(
        ISuperToken superToken,
        address constantOutflowNFTLogic,
        address constantInflowNFTLogic,
        address, // poolAdminNFTProxy,
        address // poolMemberNFT
    )
        external
        returns (
            IConstantOutflowNFT constantOutflowNFT,
            IConstantInflowNFT constantInflowNFT,
            IPoolAdminNFT poolAdminNFT,
            IPoolMemberNFT poolMemberNFT
        )
    {
        Ownable gov = Ownable(address(_host.getGovernance()));
        if (msg.sender != gov.owner()) {
            revert SUPER_TOKEN_FACTORY_ONLY_GOVERNANCE_OWNER();
        }

        string memory superTokenSymbol = superToken.symbol();

        UUPSProxy outflowNFTProxy = new UUPSProxy();
        outflowNFTProxy.initializeProxy(address(constantOutflowNFTLogic));
        constantOutflowNFT = IConstantOutflowNFT(address(outflowNFTProxy));
        constantOutflowNFT.initialize(
            superToken,
            string.concat(superTokenSymbol, " Outflow NFT"),
            string.concat(superTokenSymbol, "COF")
        );
        emit ConstantOutflowNFTCreated(constantOutflowNFT);

        UUPSProxy inflowNFTProxy = new UUPSProxy();
        inflowNFTProxy.initializeProxy(address(constantInflowNFTLogic));
        constantInflowNFT = IConstantInflowNFT(address(inflowNFTProxy));
        constantInflowNFT.initialize(
            superToken,
            string.concat(superTokenSymbol, " Inflow NFT"),
            string.concat(superTokenSymbol, "CIF")
        );
        emit ConstantInflowNFTCreated(constantInflowNFT);
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
    /* WARNING: NEVER RE-ORDER VARIABLES! Including the base contracts.
        Always double-check that new
        variables are added APPEND-ONLY. Re-ordering variables can
        permanently BREAK the deployed proxy contract. */

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
