// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import "forge-std/Test.sol";
import {
    SuperfluidFrameworkDeployer,
    TestResolver,
    SuperfluidLoader,
    TestToken
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    ERC1820RegistryCompiled
} from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";

import {
    SuperToken
} from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";

import {
    ISETH
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/tokens/ISETH.sol";
import {
    IPureSuperToken
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/tokens/IPureSuperToken.sol";

contract SuperfluidFrameworkDeployerTest is Test {
    SuperfluidFrameworkDeployer internal sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;
    TestResolver internal resolver;
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
        assertEq(resolver.get("TestGovernance.test"), address(sf.governance));
    }

    function testResolverGetsHost() public {
        assertEq(resolver.get("Superfluid.test"), address(sf.host));
    }

    function testResolverGetsLoader() public {
        assertEq(
            resolver.get("SuperfluidLoader-v1"),
            address(superfluidLoader)
        );
    }

    function testLoaderGetsFramework() public {
        SuperfluidLoader.Framework memory loadedSf = superfluidLoader
            .loadFramework("test");

        assertEq(address(loadedSf.superfluid), address(sf.host));
        assertEq(
            address(loadedSf.superTokenFactory),
            address(sf.superTokenFactory)
        );
        assertEq(address(loadedSf.agreementCFAv1), address(sf.cfa));
        assertEq(address(loadedSf.agreementIDAv1), address(sf.ida));
    }

    function testDeployWrapperSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) public {
        (TestToken underlyingToken, SuperToken superToken) = sfDeployer
            .deployWrapperSuperToken(_name, _symbol, _decimals, _mintLimit);

        // assert underlying erc20 name/symbol properly set
        assertEq(underlyingToken.name(), _name);
        assertEq(underlyingToken.symbol(), _symbol);

        // assert super token name/symbol properly set
        assertEq(superToken.name(), string.concat("Super ", _symbol));
        assertEq(superToken.symbol(), string.concat(_symbol, "x"));

        // assert proper resolver listing for underlying and wrapper super token
        address resolverUnderlyingTokenAddress = resolver.get(
            string.concat("tokens.test.", underlyingToken.symbol())
        );
        assertEq(resolverUnderlyingTokenAddress, address(underlyingToken));
        address resolverSuperTokenAddress = resolver.get(
            string.concat("supertokens.test.", superToken.symbol())
        );
        assertEq(resolverSuperTokenAddress, address(superToken));
    }

    function testDeployNativeAssetSuperToken(
        string calldata _name,
        string calldata _symbol
    ) public {
        ISETH nativeAssetSuperToken = sfDeployer.deployNativeAssetSuperToken(
            _name,
            _symbol
        );

        // assert native asset super token name/symbol properly set
        assertEq(nativeAssetSuperToken.name(), _name);
        assertEq(nativeAssetSuperToken.symbol(), _symbol);

        // assert proper resolver listing
        address resolverTokenAddress = resolver.get(
            string.concat("supertokens.test.", nativeAssetSuperToken.symbol())
        );
        assertEq(resolverTokenAddress, address(nativeAssetSuperToken));
    }

    function testDeployPureSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint256 _initialSupply
    ) public {
        vm.assume(_initialSupply <= uint256(type(int256).max));

        IPureSuperToken pureSuperToken = sfDeployer.deployPureSuperToken(
            _name,
            _symbol,
            _initialSupply
        );

        // assert pure super token name/symbol properly set
        assertEq(pureSuperToken.name(), _name);
        assertEq(pureSuperToken.symbol(), _symbol);

        // assert proper resolver listing
        address resolverTokenAddress = resolver.get(
            string.concat("supertokens.test.", pureSuperToken.symbol())
        );
        assertEq(resolverTokenAddress, address(pureSuperToken));
    }
}