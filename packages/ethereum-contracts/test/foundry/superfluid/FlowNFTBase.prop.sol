// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract FlowNFTBaseProperties is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(0) {}

    function setUp() public override {
        super.setUp();
    }

    function test_No_Token_Id_Collision_Different_Token_Same_Sender_Receiver(
        address tokenA,
        address tokenB
    ) public {
        vm.assume(tokenA != tokenB);
        uint256 tokenIdA = superToken.CONSTANT_OUTFLOW_NFT().getTokenId(
            tokenA,
            alice,
            bob
        );
        uint256 tokenIdB = superToken.CONSTANT_OUTFLOW_NFT().getTokenId(
            tokenB,
            alice,
            bob
        );
        assertTrue(
            tokenIdA != tokenIdB,
            "FlowNFTBaseProperties: Token Ids should differ"
        );
    }
}
