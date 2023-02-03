// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { Test } from "forge-std/Test.sol";

import {
    SuperfluidFrameworkDeployer,
    TestResolver,
    SuperfluidLoader
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    ERC1820RegistryCompiled
} from "../../contracts/libs/ERC1820RegistryCompiled.sol";
import {
    SuperTokenDeployer
} from "../../contracts/utils/SuperTokenDeployer.sol";

/// @title DeployerBaseTest base contract
/// @author Superfluid
/// @notice A base contract that holds a lot of shared state/initialization for Foundry tests
/// @dev This was created to eliminate duplication of setup logic in Foundry tests
contract DeployerBaseTest is Test {
    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperTokenDeployer internal immutable superTokenDeployer;

    SuperfluidFrameworkDeployer.Framework internal sf;
    TestResolver internal resolver;
    address internal constant admin = address(0x420);

    constructor() {
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);

        // deploy SuperfluidFrameworkDeployer
        // which deploys in its constructor:
        // - TestGovernance
        // - Host
        // - CFA
        // - IDA
        // - SuperTokenFactory
        // - Resolver
        // - SuperfluidLoader
        // - CFAv1Forwarder
        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();

        resolver = sf.resolver;

        // deploy SuperTokenDeployer
        superTokenDeployer = new SuperTokenDeployer(
            address(sf.superTokenFactory),
            address(sf.resolver)
        );

        // transfer ownership of TestGovernance to superTokenDeployer
        // governance ownership is required for initializing the NFT
        // contracts on the SuperToken
        sfDeployer.transferOwnership(address(superTokenDeployer));

        // add superTokenDeployer as admin to the resolver so it can register the SuperTokens
        sf.resolver.addAdmin(address(superTokenDeployer));
    }

    function setUp() public virtual {}
}
