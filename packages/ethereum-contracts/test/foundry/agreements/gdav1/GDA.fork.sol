// SPDX-License-Identifier: AGPLv3
import "forge-std/Test.sol";

import { GDAv1Forwarder, PoolConfig } from "../../../../contracts/utils/GDAv1Forwarder.sol";
import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken,
    ISuperfluidPool
} from "../../../../contracts/interfaces/superfluid/ISuperfluid.sol";

// forge test --fork-url https://bsc-dataseed.binance.org --match-contract GeneralDistributionAgreementV1ForkTest -vvvv
contract GeneralDistributionAgreementV1ForkTest is Test {
    function testUpdateMemberUnits() public {
        // just a random account with some super tokens on BNB already
        address poolAdmin = address(0xe41BA4AA270363e50b3a1B7050A57b6f8AAc5399);

        // create pool
        vm.startPrank(poolAdmin);
        (bool success, ISuperfluidPool pool) = GDAv1Forwarder(0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08).createPool(
            ISuperfluidToken(0x529A4116F160c833c61311569D6B33dFF41fD657),
            poolAdmin,
            PoolConfig({ transferabilityForUnitsOwner: false, distributionFromAnyAddress: false })
        );
        vm.stopPrank();

        // assert pool is created...
        assert(success);
        assertEq(pool.admin(), poolAdmin);
        assert(address(pool) != address(0));

        // update member units
        vm.startPrank(poolAdmin);
        pool.updateMemberUnits(poolAdmin, 100);
        vm.stopPrank();
    }
}
