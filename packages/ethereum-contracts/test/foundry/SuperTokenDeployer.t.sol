// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { DeployerBaseTest } from "./DeployerBase.t.sol";

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

contract SuperTokenDeployerTest is DeployerBaseTest {
    function setUp() public virtual override {
        super.setUp();
    }

    function testDeployWrapperSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) public {
        (TestToken underlyingToken, SuperToken superToken) = superTokenDeployer
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
        ISETH nativeAssetSuperToken = superTokenDeployer
            .deployNativeAssetSuperToken(_name, _symbol);

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

        IPureSuperToken pureSuperToken = superTokenDeployer
            .deployPureSuperToken(_name, _symbol, _initialSupply);

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
