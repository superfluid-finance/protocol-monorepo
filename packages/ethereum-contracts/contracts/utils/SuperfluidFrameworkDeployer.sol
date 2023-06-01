// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { IResolver } from "../interfaces/utils/IResolver.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";
import { Superfluid } from "../superfluid/Superfluid.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import { SuperfluidFrameworkDeploymentSteps } from "./SuperfluidFrameworkDeploymentSteps.sol";

// TODO: deploy erc1820 contract if it doesn't exist

/// @title Superfluid Framework Deployer
/// @dev This deployer should only be used for local testing environments.
contract SuperfluidFrameworkDeployer is SuperfluidFrameworkDeploymentSteps {
    struct TestFrameworkConfigs {
        // Whether the protocol is not upgradeable
        // @note we want this to be false eventually
        // Default: true
        bool nonUpgradeable;
        // Whether app whitelisting is required
        // Default: false
        bool appWhitelistingEnabled;
        // Trusted forwarders that can forward batch calls on your behalf via the Host
        // Default: []
        address[] trustedForwarders;
        // The default address to receive patrician period liquidation rewards (TOGA if unset/address(0))
        // Default: address(69)
        address defaultRewardAddress;
        // The multiplier against the flow rate which determines the buffer required for flow creation
        // Buffer = flowRate * liquidationPeriod
        // Default: 4 hours
        uint256 liquidationPeriod;
        // The amount of time where a liquidation results in a reward for the defaultRewardAddress
        // Default: 30 minutes
        uint256 patricianPeriod;
        // The minimum amount of time that a user must bond their tokens in TOGA for
        // Default: 1 week
        uint256 minBondDuration;
    }

    error DEPLOY_AGREEMENTS_REQUIRES_DEPLOY_CORE();
    error DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_CORE();
    error DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_AGREEMENTS();
    error DEPLOY_SUPER_TOKEN_CONTRACTS_REQUIRES_DEPLOY_CORE();

    /// @notice Deploys the Superfluid Framework (Test)
    /// @dev This uses default configurations for the framework.
    function deployTestFramework() external {
        // Default Configs
        TestFrameworkConfigs memory configs = TestFrameworkConfigs({
            nonUpgradeable: DEFAULT_NON_UPGRADEABLE,
            appWhitelistingEnabled: DEFAULT_APP_WHITELISTING_ENABLED,
            trustedForwarders: DEFAULT_TRUSTED_FORWARDERS,
            defaultRewardAddress: DEFAULT_REWARD_ADDRESS,
            liquidationPeriod: DEFAULT_LIQUIDATION_PERIOD,
            patricianPeriod: DEFAULT_PATRICIAN_PERIOD,
            minBondDuration: DEFAULT_TOGA_MIN_BOND_DURATION
        });

        _deployTestFramework(configs);
    }

    /// @notice Deploys the Superfluid Framework (Test) w/ Configs
    /// @dev This allows user-specified configuration for the framework.
    /// @param configs the user-defined configuration for the framework
    function deployTestFramework(TestFrameworkConfigs memory configs) external {
        _deployTestFramework(configs);
    }

    function _deployTestFramework(TestFrameworkConfigs memory configs) internal {
        // Deploy Host and Governance
        _deployCoreContracts(configs);

        // Initialize Host with Governance address
        _initializeHost();

        // Initialize Governance with Host address and Configs
        _initializeGovernance(
            configs.defaultRewardAddress, configs.liquidationPeriod, configs.patricianPeriod, configs.trustedForwarders
        );

        // Deploy CFAv1 and IDAv1
        _deployAgreementContracts();

        // Register the agreements with governance
        _registerAgreements();

        // Deploy NFT Proxy and Logic, SuperToken Logic, SuperTokenFactory Proxy and Logic contracts
        _deploySuperTokenContracts();

        // Set SuperTokenFactory as the canonical contract
        testGovernance.updateContracts(host, address(0), new address[](0), address(superTokenFactory));

        // Deploy Resolver, SuperfluidLoaderV1, CFAv1Forwarder, TOGA, BatchLiquidator contracts
        _deployPeripheralContracts(configs);

        // Enable the CFAv1Forwarder as a trusted forwarder via Governance
        _enableCFAv1ForwarderAsTrustedForwarder();

        // Set TestGovernance, Superfluid, SuperfluidLoader and CFAv1Forwarder addresses in Resolver
        _setAddressesInResolver();
    }

    /// @notice Deploys the core Superfluid contracts
    /// @dev Host and Governance
    function deployCoreContracts() public {
        TestFrameworkConfigs memory configs;
        configs.nonUpgradeable = true;
        configs.appWhitelistingEnabled = false;

        _deployCoreContracts(configs);
    }

    /// @notice Deploys the core Superfluid contracts w/ Configs
    /// @dev Host and Governance
    /// @param configs the configurations for the framework
    function deployCoreContracts(TestFrameworkConfigs memory configs) public {
        _deployCoreContracts(configs);
    }

    function _deployCoreContracts(TestFrameworkConfigs memory configs) internal {
        _deployGovernance(address(this));
        _deployHost(configs.nonUpgradeable, configs.appWhitelistingEnabled);
    }

    /// @notice Deploys the Superfluid agreement contracts
    /// @dev Deploys Superfluid agreement contracts
    /// NOTE: This requires the core contracts to be deployed first.
    function deployAgreementContracts() public {
        _deployAgreementContracts();
    }

    function _deployAgreementContracts() internal {
        if (address(host) == address(0)) revert DEPLOY_AGREEMENTS_REQUIRES_DEPLOY_CORE();

        _deployCFAv1();
        _deployIDAv1();
    }

    /// @notice Deploys all SuperToken-related contracts
    /// @dev Deploys NFT Proxy and Logic, SuperToken Logic, SuperTokenFactory Proxy and Logic contracts
    function deploySuperTokenContracts() public {
        _deploySuperTokenContracts();
    }

    function _deploySuperTokenContracts() internal {
        if (address(host) == address(0)) revert DEPLOY_SUPER_TOKEN_CONTRACTS_REQUIRES_DEPLOY_CORE();

        _deployNFTProxyAndLogicAndInitialize();
        _deploySuperTokenLogic();
        _deploySuperTokenFactory();
    }

    /// @notice Deploys all peripheral Superfluid contracts
    /// @dev Deploys Resolver, SuperfluidLoaderV1, CFAv1Forwarder, TOGA, BatchLiquidator contracts
    function deployPeripheralContracts() public {
        TestFrameworkConfigs memory configs;
        configs.minBondDuration = DEFAULT_TOGA_MIN_BOND_DURATION;

        _deployPeripheralContracts(configs);
    }

    /// @notice Deploys all peripheral Superfluid contracts with configs
    /// @dev Deploys Resolver, SuperfluidLoaderV1, CFAv1Forwarder, TOGA, BatchLiquidator contracts
    function deployPeripheralContracts(TestFrameworkConfigs memory configs) public {
        _deployPeripheralContracts(configs);
    }

    function _deployPeripheralContracts(TestFrameworkConfigs memory configs) internal {
        if (address(host) == address(0)) revert DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_CORE();

        _deployTestResolver(address(this));
        _deploySuperfluidLoader();

        // Set the deployer of this contract as an admin of the resolver
        // So that they can add other admins and set addresses
        testResolver.addAdmin(msg.sender);

        _deployCFAv1Forwarder();
        _deployTOGA(configs.minBondDuration);

        if (address(cfaV1) == address(0)) revert DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_AGREEMENTS();
        _deployBatchLiquidator();
    }
}
