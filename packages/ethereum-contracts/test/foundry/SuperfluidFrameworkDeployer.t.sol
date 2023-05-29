// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "./FoundrySuperfluidTester.sol";
import { SuperfluidLoader } from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import { ERC1820RegistryCompiled } from "../../contracts/libs/ERC1820RegistryCompiled.sol";

contract SuperfluidFrameworkDeployerTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(1) { }

    function testAllContractsDeployed() public {
        assertTrue(address(sf.governance) != address(0), "SFDeployer: governance not deployed");
        assertTrue(address(sf.host) != address(0), "SFDeployer: host not deployed");
        assertTrue(address(sf.cfa) != address(0), "SFDeployer: cfa not deployed");
        assertTrue(address(sf.ida) != address(0), "SFDeployer: ida not deployed");
        assertTrue(address(sf.superTokenFactory) != address(0), "SFDeployer: superTokenFactory not deployed");
        assertTrue(address(sf.resolver) != address(0), "SFDeployer: resolver not deployed");
        assertTrue(address(sf.superfluidLoader) != address(0), "SFDeployer: superfluidLoader not deployed");
        assertTrue(address(sf.cfaV1Forwarder) != address(0), "SFDeployer: cfaV1Forwarder not deployed");
    }

    function testResolverGetsGovernance() public {
        assertEq(resolver.get("TestGovernance.test"), address(sf.governance), "SFDeployer: governance not registered");
    }

    function testResolverGetsHost() public {
        assertEq(resolver.get("Superfluid.test"), address(sf.host), "SFDeployer: host not registered");
    }

    function testResolverGetsLoader() public {
        assertEq(
            resolver.get("SuperfluidLoader-v1"),
            address(sf.superfluidLoader),
            "SFDeployer: superfluidLoader not registered"
        );
    }

    function testLoaderGetsFramework() public {
        SuperfluidLoader.Framework memory loadedSf = sf.superfluidLoader.loadFramework("test");

        assertEq(address(loadedSf.superfluid), address(sf.host), "SFDeployer: host not loaded");
        assertEq(address(loadedSf.agreementCFAv1), address(sf.cfa), "SFDeployer: cfa not loaded");
        assertEq(address(loadedSf.agreementIDAv1), address(sf.ida), "SFDeployer: ida not loaded");
    }

    function testTransferOwnership() public {
        assertEq(sf.governance.owner(), address(sfDeployer), "SFDeployer: governance not owned by deployer");
        sfDeployer.transferOwnership(address(superTokenDeployer));
        assertEq(
            sf.governance.owner(), address(superTokenDeployer), "SFDeployer: governance not owned by superTokenDeployer"
        );
    }
}
