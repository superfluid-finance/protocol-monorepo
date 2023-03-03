// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { FoundrySuperfluidTester } from "./FoundrySuperfluidTester.sol";
import {
    SuperfluidLoader
} from "../../contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    ERC1820RegistryCompiled
} from "../../contracts/libs/ERC1820RegistryCompiled.sol";

contract SuperfluidFrameworkDeployerTest is FoundrySuperfluidTester {

    constructor() FoundrySuperfluidTester(1) {}

    function test_Passing_All_Contracts_Deployed() public {
        assertTrue(address(sf.governance) != address(0));
        assertTrue(address(sf.host) != address(0));
        assertTrue(address(sf.cfa) != address(0));
        assertTrue(address(sf.ida) != address(0));
        assertTrue(address(sf.superTokenFactory) != address(0));
        assertTrue(address(sf.resolver) != address(0));
        assertTrue(address(sf.superfluidLoader) != address(0));
        assertTrue(address(sf.cfaV1Forwarder) != address(0));
    }

    function test_Passing_Resolver_Gets_Governance() public {
        assertEq(resolver.get("TestGovernance.test"), address(sf.governance));
    }

    function test_Passing_Resolver_Gets_Host() public {
        assertEq(resolver.get("Superfluid.test"), address(sf.host));
    }

    function test_Passing_Resolver_Gets_Loader() public {
        assertEq(
            resolver.get("SuperfluidLoader-v1"),
            address(sf.superfluidLoader)
        );
    }

    function test_Passing_Loader_Gets_Framework() public {
        SuperfluidLoader.Framework memory loadedSf = sf
            .superfluidLoader
            .loadFramework("test");

        assertEq(address(loadedSf.superfluid), address(sf.host));
        assertEq(address(loadedSf.agreementCFAv1), address(sf.cfa));
        assertEq(address(loadedSf.agreementIDAv1), address(sf.ida));
    }

    function test_Passing_Transfer_Ownership() public {
        assertEq(sf.governance.owner(), address(sfDeployer));
        sfDeployer.transferOwnership(address(superTokenDeployer));
        assertEq(sf.governance.owner(), address(superTokenDeployer));
    }
}
