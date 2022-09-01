// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

import "forge-std/Test.sol";

import {
    Superfluid,
    ConstantFlowAgreementV1,
    InstantDistributionAgreementV1,
    ERC20PresetMinterPauser,
    SuperToken,
    SuperfluidFrameworkDeployer,
    CFAv1Library,
    IDAv1Library
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";


contract FoundrySuperfluidTester is Test {

    uint internal constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint internal constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address internal constant admin = address(0x420);
    address internal constant alice = address(0x421);
    address internal constant bob = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant dan = address(0x424);
    address internal constant eve = address(0x425);
    address internal constant frank = address(0x426);
    address internal constant grace = address(0x427);
    address internal constant heidi = address(0x428);
    address internal constant ivan = address(0x429);
    address[] internal TEST_ACCOUNTS = [admin,alice,bob,carol,dan,eve,frank,grace,heidi,ivan];

    uint internal immutable N_TESTERS;
    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;

    ERC20PresetMinterPauser internal token;
    SuperToken internal superToken;

    uint256 private _expectedTotalSupply;

    constructor (uint8 nTesters) {
        require(nTesters <= TEST_ACCOUNTS.length, "too many testers");
        N_TESTERS = nTesters;

        vm.startPrank(admin);

        // Deploy ERC1820
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);

        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();

        vm.stopPrank();
    }

    function setUp() virtual public {
        (token, superToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT");

        for (uint i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);

            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }
    }

    function checkAllInvariants() public view returns (bool) {
        return
            checkLiquiditySumInvariance() &&
            checkNetFlowRateSumInvariant();
    }

    function checkLiquiditySumInvariance() public view returns (bool) {
        int256 liquiditySum;
        for (uint i = 0; i < TEST_ACCOUNTS.length; ++i) {
            (int256 avb, uint256 d, uint256 od, ) = superToken.realtimeBalanceOfNow(address(TEST_ACCOUNTS[i]));
            liquiditySum += avb + int256(d) - int256(od);
        }
        return int256(_expectedTotalSupply) == liquiditySum;
    }

    function checkNetFlowRateSumInvariant() public view returns (bool) {
        int96 netFlowRateSum;
        for (uint i = 0; i < TEST_ACCOUNTS.length; ++i) {
            netFlowRateSum += sf.cfa.getNetFlow(superToken, address(TEST_ACCOUNTS[i]));
        }
        return netFlowRateSum == 0;
    }

}
