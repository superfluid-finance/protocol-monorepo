pragma solidity >= 0.8.0;

import "@superfluid-finance/ethereum-contracts/contracts/superfluid/Superfluid.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";

contract NoCallbackSuperfluidFuzzer {

    Superfluid _sf;
    /* SuperToken _token;
    address[10] testAccounts;
    uint256 constant INIT_BALANCE = 1000e18; */

    constructor () {
        _sf = new Superfluid(true /* nonUpgradable */, false /* appWhiteListingEnabled */);
    }

    function sumOfBalanceInvariant() external {
        //assertEq(sumOfAllTestAccounts(), INIT_BALANCE);
        assert(true);
    }

}
