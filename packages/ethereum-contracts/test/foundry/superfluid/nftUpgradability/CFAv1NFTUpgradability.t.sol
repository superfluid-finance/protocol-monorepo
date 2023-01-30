// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { Test } from "forge-std/Test.sol";

import { UUPSProxy } from "../../../../contracts/upgradability/UUPSProxy.sol";

import {
    CFAv1NFTBaseMockV1,
    CFAv1NFTBaseMockV1BadPreGap,
    CFAv1NFTBaseMockV1BadPostGap,
    CFAv1NFTBaseMockV1Good,
    ConstantOutflowNFTMockV1,
    ConstantOutflowNFTMockV1Bad,
    ConstantOutflowNFTMockV1Good
} from "./CFAv1NFTMocks.sol";

// Should be able to update CFAv1NFTBase by adding new storage variables in gap space
// Should not be able to update CFAv1NFTBase by adding new storage variables in between existing storage
// Should not be able to update CFAv1NFTBase by adding new storage variables after gap space
// It breaks ConstantOutflowNFT and breaks ConstantInflowNFT if we upgraded it to have a single storage variable

// Should be able to update ConstantOutflowNFT by adding new storage variables after mapping
// Should not be able to update ConstantOutflowNFT by adding new storage variables before mapping

// Should be able to update ConstantInflowNFT by adding new storage variables after mapping

/// @title ConstantFAv1NFTsUpgradabilityTest
/// @author Superfluid
/// @notice Used for testing upgradability of CFAv1 NFT contracts
/// @dev Add a test for new NFT logic contracts here when it changes
contract ConstantFAv1NFTsUpgradabilityTest is Test {
    UUPSProxy proxy;
    CFAv1NFTBaseMockV1 cfaV1NFTBaseMockV1Logic;
    CFAv1NFTBaseMockV1 cfaV1NFTBaseMockV1Proxy;

    function setUp() public {
        proxy = new UUPSProxy();
        cfaV1NFTBaseMockV1Logic = new CFAv1NFTBaseMockV1();
        proxy.initializeProxy(address(cfaV1NFTBaseMockV1Logic));
        cfaV1NFTBaseMockV1Proxy = CFAv1NFTBaseMockV1(address(proxy));
    }

    function test_Passing_CFAv1NFTBaseMockV1ProxyInitialization() external {
        assertEq(
            cfaV1NFTBaseMockV1Proxy.getCodeAddress(),
            address(cfaV1NFTBaseMockV1Logic)
        );
    }

    function test_Passing_CFAv1NFTBaseMockV1ValidateBaseLayout() external {
        cfaV1NFTBaseMockV1Logic.validateStorageLayout();
    }
}
