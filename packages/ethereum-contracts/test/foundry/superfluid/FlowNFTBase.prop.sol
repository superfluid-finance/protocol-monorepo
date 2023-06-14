// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract FlowNFTBasePropertyTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) { }

    function setUp() public override {
        super.setUp();
    }

    function testNoTokenIdCollisionDifferentTokenSameSenderReceiver(address tokenA, address tokenB) public {
        vm.assume(tokenA != tokenB);
        uint256 tokenIdA = superToken.CONSTANT_OUTFLOW_NFT().getTokenId(tokenA, alice, bob);
        uint256 tokenIdB = superToken.CONSTANT_OUTFLOW_NFT().getTokenId(tokenB, alice, bob);
        assertNotEq(tokenIdA, tokenIdB, "FlowNFTBaseProperties: Token Ids should differ");
    }

    function testBalanceOfIsAlwaysEqualToOne(address account) public {
        uint256 balance = superToken.CONSTANT_OUTFLOW_NFT().balanceOf(account);
        assertEq(balance, 1, "FlowNFTBaseProperties: Balance of should always be equal to one");
    }
}
