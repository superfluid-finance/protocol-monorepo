// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import "forge-std/Test.sol";
import {
    SuperfluidFrameworkDeployer,
    Resolver,
    SuperfluidLoader
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    ERC1820RegistryCompiled
} from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";

contract SuperfluidFrameworkDeployerTest is Test {
    SuperfluidFrameworkDeployer internal sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;
    Resolver internal resolver;
    SuperfluidLoader internal superfluidLoader; 

    address internal constant admin = address(0x420);

    function setUp() public {
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);

        sfDeployer = new SuperfluidFrameworkDeployer();

        sf = sfDeployer.getFramework();

        resolver = sf.resolver;

        superfluidLoader = sf.superfluidLoader;
    }

    function testAllContractsDeployed() public {
        assertTrue(address(sf.governance) != address(0));
        assertTrue(address(sf.host) != address(0));
        assertTrue(address(sf.cfa) != address(0));
        assertTrue(address(sf.ida) != address(0));
        assertTrue(address(sf.superTokenFactory) != address(0));
        assertTrue(address(sf.resolver) != address(0));
        assertTrue(address(sf.superfluidLoader) != address(0));
    }

    function testResolverGetsGovernance() public {
        assertEq(
            resolver.get("TestGovernance.test"),
            address(sf.governance)
        );
    }

    function testResolverGetsHost() public {
        assertEq(
            resolver.get("Superfluid.test"),
            address(sf.host)
        );
    }

    function testResolverGetsLoader() public {
        assertEq(
            resolver.get("SuperfluidLoader-v1"),
            address(superfluidLoader)
        );
    }

    function testLoaderGetsFramework() public {
        SuperfluidLoader.Framework memory loadedSf = superfluidLoader.loadFramework("test");

        assertEq(address(loadedSf.superfluid), address(sf.host));
        assertEq(address(loadedSf.superTokenFactory), address(sf.superTokenFactory));
        assertEq(address(loadedSf.agreementCFAv1), address(sf.cfa));
        assertEq(address(loadedSf.agreementIDAv1), address(sf.ida));
    }
}
