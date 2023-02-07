// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import "../FoundrySuperfluidTester.sol";

import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper
} from "../../../contracts/superfluid/SuperTokenFactory.sol";

contract ConstantFlowAgreementV1Anvil is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    constructor() FoundrySuperfluidTester(3) {}

    function test_Fuzz_Revert_If_Update_Logic_Contracts_Is_Not_Called_By_Self(
        address caller
    ) public {
        vm.assume(caller != address(sf.superTokenFactory));
        vm.prank(caller);
        vm.expectRevert(
            ISuperTokenFactory.SUPER_TOKEN_FACTORY_ONLY_SELF.selector
        );
        sf.superTokenFactory.updateLogicContracts();
    }

    function test_Passing_If_Update_Logic_Contracts_Is_Called_By_Self() public {
        vm.prank(address(sf.superTokenFactory));
        sf.superTokenFactory.updateLogicContracts();
    }
}
