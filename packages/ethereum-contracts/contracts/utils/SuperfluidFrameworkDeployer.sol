// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    SuperfluidGovDeployerLibrary
} from "./deployers/SuperfluidGovDeployerLibrary.sol";
import {
    SuperfluidHostDeployerLibrary
} from "./deployers/SuperfluidHostDeployerLibrary.sol";
import {
    SuperfluidCFAv1DeployerLibrary
} from "./deployers/SuperfluidCFAv1DeployerLibrary.sol";
import {
    SuperfluidIDAv1DeployerLibrary
} from "./deployers/SuperfluidIDAv1DeployerLibrary.sol";
import {
    SuperfluidSuperTokenFactoryHelperDeployerLibrary
} from "./deployers/SuperfluidSuperTokenFactoryHelperDeployerLibrary.sol";
import {
    SuperfluidPeripheryDeployerLibrary
} from "./deployers/SuperfluidPeripheryDeployerLibrary.sol";
import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import { Superfluid } from "../superfluid/Superfluid.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { TestGovernance } from "./TestGovernance.sol";
import {
    ConstantFlowAgreementV1
} from "../agreements/ConstantFlowAgreementV1.sol";
import {
    InstantDistributionAgreementV1
} from "../agreements/InstantDistributionAgreementV1.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import { SETHProxy } from "../tokens/SETH.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";
import {
    IConstantFlowAgreementHook
} from "../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { IPureSuperToken } from "../interfaces/tokens/IPureSuperToken.sol";
import { ISETH } from "../interfaces/tokens/ISETH.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";
import { TestToken } from "./TestToken.sol";

/// @title Superfluid Framework Deployer
/// @notice This is NOT for deploying public nets, but rather only for tesing envs
contract SuperfluidFrameworkDeployer {
    string public constant RESOLVER_BASE_SUPER_TOKEN_KEY = "supertokens.test.";
    string public constant RESOLVER_BASE_TOKEN_KEY = "tokens.test.";

    struct Framework {
        TestGovernance governance;
        Superfluid host;
        ConstantFlowAgreementV1 cfa;
        CFAv1Library.InitData cfaLib;
        InstantDistributionAgreementV1 ida;
        IDAv1Library.InitData idaLib;
        SuperTokenFactory superTokenFactory;
        TestResolver resolver;
        SuperfluidLoader superfluidLoader;
        CFAv1Forwarder cfaV1Forwarder;
    }

    TestGovernance internal testGovernance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfaV1;
    InstantDistributionAgreementV1 internal idaV1;
    SuperTokenFactory internal superTokenFactory;
    TestResolver internal testResolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;

    constructor() {
        // @note ERC1820 must be deployed for this to work

        // Deploy TestGovernance. Needs initialization later.
        testGovernance = SuperfluidGovDeployerLibrary.deployTestGovernance();

        // Transfer ownership to this contract
        SuperfluidGovDeployerLibrary.transferOwnership(
            testGovernance,
            address(this)
        );

        // Deploy Superfluid
        host = SuperfluidHostDeployerLibrary.deploySuperfluidHost(true, false);

        // Initialize Superfluid with Governance address
        host.initialize(testGovernance);

        // Initialize Governance
        address[] memory trustedForwarders = new address[](0);
        testGovernance.initialize(
            host,
            address(69),
            4 hours,
            30 minutes,
            trustedForwarders
        );

        // Deploy ConstantFlowAgreementV1
        // TODO @note Once we have the actual implementation for the hook contract,
        // we will need to deploy it and put it here

        cfaV1 = SuperfluidCFAv1DeployerLibrary.deployConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0))
        );

        // Register ConstantFlowAgreementV1 TestGovernance
        testGovernance.registerAgreementClass(host, address(cfaV1));

        // Deploy CFAv1Forwarder
        cfaV1Forwarder = new CFAv1Forwarder(host);

        // Enable CFAv1Forwarder as a Trusted Forwarder
        testGovernance.enableTrustedForwarder(
            host,
            ISuperfluidToken(address(0)),
            address(cfaV1Forwarder)
        );

        // Deploy InstantDistributionAgreementV1
        idaV1 = SuperfluidIDAv1DeployerLibrary
            .deployInstantDistributionAgreementV1(host);

        // Register InstantDistributionAgreementV1 with Governance
        testGovernance.registerAgreementClass(host, address(idaV1));

        // Deploy SuperTokenFactoryHelper
        SuperTokenFactoryHelper superTokenFactoryHelper = SuperfluidSuperTokenFactoryHelperDeployerLibrary
                .deploySuperTokenFactoryHelper();

        // Deploy SuperTokenFactory
        superTokenFactory = SuperfluidPeripheryDeployerLibrary
            .deploySuperTokenFactory(host, superTokenFactoryHelper);

        // 'Update' code with Governance and register SuperTokenFactory with Superfluid
        testGovernance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(superTokenFactory)
        );

        // Deploy Resolver and grant the deployer of SuperfluidFrameworkDeployer admin privileges
        testResolver = SuperfluidPeripheryDeployerLibrary.deployTestResolver(
            msg.sender
        );

        // Deploy SuperfluidLoader
        superfluidLoader = new SuperfluidLoader(testResolver);

        // Register Governance with Resolver
        testResolver.set("TestGovernance.test", address(testGovernance));

        // Register Superfluid with Resolver
        testResolver.set("Superfluid.test", address(host));

        // Register SuperfluidLoader with Resolver
        testResolver.set("SuperfluidLoader-v1", address(superfluidLoader));

        testResolver.set("CFAv1Forwarder", address(cfaV1Forwarder));
    }

    /// @notice Fetches the framework contracts
    function getFramework() external view returns (Framework memory sf) {
        sf = Framework({
            governance: testGovernance,
            host: host,
            cfa: cfaV1,
            cfaLib: CFAv1Library.InitData(host, cfaV1),
            ida: idaV1,
            idaLib: IDAv1Library.InitData(host, idaV1),
            superTokenFactory: superTokenFactory,
            resolver: testResolver,
            superfluidLoader: superfluidLoader,
            cfaV1Forwarder: cfaV1Forwarder
        });
        return sf;
    }

    /// @notice Deploys an ERC20 and a Wrapper Super Token for the ERC20 and lists both in the resolver
    /// @dev SuperToken name and symbol format: `Super ${_underlyingSymbol}` and `${_underlyingSymbol}x`, respectively
    /// @param _underlyingName The underlying token name
    /// @param _underlyingSymbol The token symbol
    /// @return underlyingToken and superToken
    function deployWrapperSuperToken(
        string calldata _underlyingName,
        string calldata _underlyingSymbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) external returns (TestToken underlyingToken, SuperToken superToken) {
        underlyingToken = new TestToken(
            _underlyingName,
            _underlyingSymbol,
            _decimals,
            _mintLimit
        );
        superToken = SuperToken(
            address(
                superTokenFactory.createERC20Wrapper(
                    ERC20WithTokenInfo(address(underlyingToken)),
                    ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
                    string.concat("Super ", _underlyingSymbol),
                    string.concat(_underlyingSymbol, "x")
                )
            )
        );

        // list underlying token in resolver
        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_TOKEN_KEY, underlyingToken.symbol()),
            address(underlyingToken)
        );

        // list super token in resolver
        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, superToken.symbol()),
            address(superToken)
        );
    }

    /// @notice Deploys a Native Asset Super Token and lists it in the resolver
    /// @dev e.g. ETHx, MATICx, AVAXx, etc. The underlying is the Native Asset.
    /// @param _name The token name
    /// @param _symbol The token symbol
    /// @return nativeAssetSuperToken
    function deployNativeAssetSuperToken(
        string calldata _name,
        string calldata _symbol
    ) external returns (ISETH nativeAssetSuperToken) {
        SETHProxy sethProxy = new SETHProxy();
        nativeAssetSuperToken = ISETH(address(sethProxy));
        superTokenFactory.initializeCustomSuperToken(address(sethProxy));
        nativeAssetSuperToken.initialize(
            IERC20(address(0)),
            18,
            _name,
            _symbol
        );

        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, _symbol),
            address(nativeAssetSuperToken)
        );
    }

    /// @notice Deploys a Pure Super Token and lists it in the resolver
    /// @dev We specify the initial supply (because non-downgradeable) on creation and send it to the deployer
    /// @param _name The token name
    /// @param _symbol The token symbol
    /// @param _initialSupply The initial token supply of the pure super token
    /// @return pureSuperToken
    function deployPureSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint256 _initialSupply
    ) external returns (IPureSuperToken pureSuperToken) {
        PureSuperToken pureSuperTokenProxy = new PureSuperToken();
        superTokenFactory.initializeCustomSuperToken(
            address(pureSuperTokenProxy)
        );
        pureSuperTokenProxy.initialize(_name, _symbol, _initialSupply);

        pureSuperToken = IPureSuperToken(address(pureSuperTokenProxy));

        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, _symbol),
            address(pureSuperToken)
        );

        // transfer initial supply to deployer
        pureSuperToken.transfer(msg.sender, _initialSupply);
    }

    function _handleResolverList(
        bool _listOnResolver,
        string memory _resolverKey,
        address _superTokenAddress
    ) internal {
        if (_listOnResolver) {
            testResolver.set(_resolverKey, address(_superTokenAddress));
        }
    }
}