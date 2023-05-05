// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "./FoundrySuperfluidTester.sol";

import {
    IPureSuperToken,
    ISETH,
    TestToken,
    SuperToken,
    SuperTokenDeployer
} from "../../contracts/utils/SuperTokenDeployer.sol";
import {
    ERC1820RegistryCompiled
} from "../../contracts/libs/ERC1820RegistryCompiled.sol";

contract SuperTokenDeployerTest is FoundrySuperfluidTester {

    constructor() FoundrySuperfluidTester(1) {}
    function setUp() public virtual override {
        super.setUp();
    }

    function testDeployWrapperSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) public {
        (TestToken underlyingToken, SuperToken _superToken) = superTokenDeployer
            .deployWrapperSuperToken(_name, _symbol, _decimals, _mintLimit);

        // assert underlying erc20 name/symbol properly set
        assertEq(underlyingToken.name(), _name, "SuperTokenDeployer: Underlying token name not properly set");
        assertEq(underlyingToken.symbol(), _symbol, "SuperTokenDeployer: Underlying token symbol not properly set");

        // assert super token name/symbol properly set
        assertEq(_superToken.name(), string.concat("Super ", _symbol), "SuperTokenDeployer: Super token name not properly set");
        assertEq(_superToken.symbol(), string.concat(_symbol, "x"), "SuperTokenDeployer: Super token symbol not properly set");

        // assert proper resolver listing for underlying and wrapper super token
        address resolverUnderlyingTokenAddress = resolver.get(
            string.concat("tokens.test.", underlyingToken.symbol())
        );
        assertEq(resolverUnderlyingTokenAddress, address(underlyingToken), "SuperTokenDeployer: Underlying token not properly registered");
        address resolverSuperTokenAddress = resolver.get(
            string.concat("supertokens.test.", _superToken.symbol())
        );
        assertEq(resolverSuperTokenAddress, address(_superToken), "SuperTokenDeployer: Super token not properly registered");
    }

    function testDeployNativeAssetSuperToken(
        string calldata _name,
        string calldata _symbol
    ) public {
        ISETH nativeAssetSuperToken = superTokenDeployer
            .deployNativeAssetSuperToken(_name, _symbol);

        // assert native asset super token name/symbol properly set
        assertEq(nativeAssetSuperToken.name(), _name, "SuperTokenDeployer: Native asset super token name not properly set");
        assertEq(nativeAssetSuperToken.symbol(), _symbol, "SuperTokenDeployer: Native asset super token symbol not properly set");

        // assert proper resolver listing
        address resolverTokenAddress = resolver.get(
            string.concat("supertokens.test.", nativeAssetSuperToken.symbol())
        );
        assertEq(resolverTokenAddress, address(nativeAssetSuperToken), "SuperTokenDeployer: Native asset super token not properly registered");
    }

    function testDeployPureSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint256 _initialSupply
    ) public {
        vm.assume(_initialSupply <= uint256(type(int256).max));

        IPureSuperToken pureSuperToken = superTokenDeployer
            .deployPureSuperToken(_name, _symbol, _initialSupply);

        // assert pure super token name/symbol properly set
        assertEq(pureSuperToken.name(), _name, "SuperTokenDeployer: Pure super token name not properly set");
        assertEq(pureSuperToken.symbol(), _symbol, "SuperTokenDeployer: Pure super token symbol not properly set");

        // assert proper resolver listing
        address resolverTokenAddress = resolver.get(
            string.concat("supertokens.test.", pureSuperToken.symbol())
        );
        assertEq(resolverTokenAddress, address(pureSuperToken), "SuperTokenDeployer: Pure super token not properly registered");
    }
}
