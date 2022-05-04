// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
// solhint-disable func-name-mixedcase
pragma solidity >= 0.8.0;

import {
    Superfluid,
    ConstantFlowAgreementV1,
    InstantDistributionAgreementV1,
    ERC20PresetMinterPauser,
    SuperToken,
    SuperfluidFrameworkDeployer
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";

import {
    IERC20,
    ISuperToken,
    IConstantFlowAgreementV1,
    IInstantDistributionAgreementV1,
    SuperfluidTester
} from "./SuperfluidTester.sol";


contract HotFuzzBase {
    // constants
    uint private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    uint private constant N_TEST_ACCOUNTS = 5;

    // immutables
    SuperfluidFrameworkDeployer private immutable _sfDeployer;
    Superfluid internal immutable host;
    ConstantFlowAgreementV1 internal immutable cfa;
    InstantDistributionAgreementV1 internal immutable ida;
    ERC20PresetMinterPauser internal immutable token;
    SuperToken internal immutable superToken;
    SuperfluidTester[N_TEST_ACCOUNTS] internal testers;

    // test states
    uint256 private _expectedTotalSupply = 0;

    constructor() {
        _sfDeployer = new SuperfluidFrameworkDeployer();
        (host, cfa, ida, ) = _sfDeployer.getFramework();

        (token, superToken) = _sfDeployer.deployWrapperSuperToken("HOTfuzz Token", "HOTT");

        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            testers[i] = new SuperfluidTester(host, cfa, ida, token, superToken);
            token.mint(address(testers[i]), INIT_TOKEN_BALANCE);
            testers[i].upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
        }
    }

    /**************************************************************************
     * IHotFuzz implementation
     **************************************************************************/

    function getOneTester(uint8 a)
        internal view
        returns (SuperfluidTester testerA) {
        testerA = testers[a % N_TEST_ACCOUNTS];
    }

    function getTwoTesters(uint8 a, uint8 b)
        internal view
        returns (SuperfluidTester testerA, SuperfluidTester testerB) {
        testerA = testers[a % N_TEST_ACCOUNTS];
        // avoid tester B to be the same as tester A
        testerB = testers[((a % N_TEST_ACCOUNTS) + (b % (N_TEST_ACCOUNTS - 1))) % N_TEST_ACCOUNTS];
    }

    function superTokenBalanceOfNow(address a) internal view returns (int256 avb) {
        (avb,,,) = superToken.realtimeBalanceOfNow(a);
    }

    /* function upgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getOneTester(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.upgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        _expectedTotalSupply += amount;
    }

    function downgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getOneTester(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        require(a1 >= int256(uint256(amount)));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.downgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        _expectedTotalSupply -= amount;
    } */

    /**************************************************************************
     * Invariances
     **************************************************************************/

    function echidna_check_total_supply() public view returns (bool) {
        assert(superToken.totalSupply() == _expectedTotalSupply);
        return superToken.totalSupply() == _expectedTotalSupply;
    }

    function echidna_check_liquiditySumInvariance() public view returns (bool) {
        int256 liquiditySum = 0;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            (int256 avb, uint256 d, uint256 od, ) = superToken.realtimeBalanceOfNow(address(testers[i]));
            liquiditySum += avb + int256(d) - int256(od);
        }
        assert(int256(_expectedTotalSupply) == liquiditySum);
        return int256(_expectedTotalSupply) == liquiditySum;
    }

    function echidna_check_netFlowRateSumInvariant() public view returns (bool) {
        int96 netFlowRateSum = 0;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            netFlowRateSum += cfa.getNetFlow(superToken, address(testers[i]));
        }
        assert(netFlowRateSum == 0);
        return netFlowRateSum == 0;
    }
}
