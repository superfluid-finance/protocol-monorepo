// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
// solhint-disable func-name-mixedcase
pragma solidity >= 0.8.0;

import "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";

import "@superfluid-finance/ethereum-contracts/contracts/superfluid/Superfluid.sol";
import "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperToken.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";
import "@superfluid-finance/ethereum-contracts/contracts/utils/TestGovernance.sol";

import "./SuperfluidTester.sol";

abstract contract AbstractBaseFuzzer {

    function token() virtual internal view returns (IERC20);
    function superToken() virtual internal view returns (ISuperToken);
    function cfa() virtual internal view returns (IConstantFlowAgreementV1);
    function ida() virtual internal view returns (IInstantDistributionAgreementV1);

    function getTester1(uint8 a) virtual internal view
        returns (SuperfluidTester testerA);

    function getTester2(uint8 a, uint8 b) virtual internal view
        returns (SuperfluidTester testerA, SuperfluidTester testerB);

    function superTokenBalanceOfNow(address a) virtual internal view returns (int256);
}

contract BaseFuzzer is AbstractBaseFuzzer {

    uint private constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint private constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    uint private constant N_TEST_ACCOUNTS = 5;

    Superfluid private immutable _host;
    TestGovernance private immutable _gov;
    ConstantFlowAgreementV1 private immutable _cfa;
    InstantDistributionAgreementV1 private immutable _ida;
    ERC20PresetMinterPauser private immutable _token;
    ISuperToken private immutable _superToken;
    SuperfluidTester[N_TEST_ACCOUNTS] private _testers;

    // test state
    uint256 private _expectedTotalSupply = INIT_SUPER_TOKEN_BALANCE * N_TEST_ACCOUNTS;

    constructor() {
        _host = new Superfluid(
            true, // nonUpgradable,
            false // appWhiteListingEnabled
            );

        _gov = new TestGovernance();
        _host.initialize(_gov);
        _gov.initialize(_host, address(this), 3600, 720, new address[](0));

        _cfa = new ConstantFlowAgreementV1(_host);
        _gov.registerAgreementClass(_host, address(_cfa));

        _ida = new InstantDistributionAgreementV1(_host);
        _gov.registerAgreementClass(_host, address(_ida));

        _token = new ERC20PresetMinterPauser("FTT", "FTT");

        _superToken = new SuperToken(_host);

        _superToken.initialize(
            _token,
            18,
            "FTTx",
            "FTTx");

        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            _testers[i] = new SuperfluidTester(_host, _cfa, _ida, _token, _superToken);
            _token.mint(address(_testers[i]), INIT_TOKEN_BALANCE);
            _testers[i].upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
        }
    }

    function token() override internal view returns (IERC20) { return _token; }
    function superToken() override internal view returns (ISuperToken) { return _superToken; }
    function cfa() override internal view returns (IConstantFlowAgreementV1) { return _cfa; }
    function ida() override internal view returns (IInstantDistributionAgreementV1) { return _ida; }

    function getTester1(uint8 a)
        override internal view
        returns (SuperfluidTester testerA) {
        testerA = _testers[a % N_TEST_ACCOUNTS];
    }

    function getTester2(uint8 a, uint8 b)
        override internal view
        returns (SuperfluidTester testerA, SuperfluidTester testerB) {
        testerA = _testers[a % N_TEST_ACCOUNTS];
        // avoid tester B to be the same as tester A
        testerB = _testers[((a % N_TEST_ACCOUNTS) + (b % (N_TEST_ACCOUNTS - 1))) % N_TEST_ACCOUNTS];
    }

    function superTokenBalanceOfNow(address a) override internal view returns (int256 avb) {
        (avb,,,) = _superToken.realtimeBalanceOfNow(a);
    }

    function upgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getTester1(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        int256 b1 = int256(_token.balanceOf(address(tester)));
        tester.upgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(_token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        _expectedTotalSupply += amount;
    }

    function downgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getTester1(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        int256 b1 = int256(_token.balanceOf(address(tester)));
        tester.downgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(_token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        _expectedTotalSupply -= amount;
    }

    function echidna_check_total_supply() public view returns (bool) {
        return _superToken.totalSupply() == _expectedTotalSupply;
    }

    function echidna_check_liquiditySumInvariance() public view returns (bool) {
        int256 liquiditySum;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            (int256 avb, uint256 d, uint256 od, ) = _superToken.realtimeBalanceOfNow(address(_testers[i]));
            liquiditySum += avb + int256(d) - int256(od);
        }
        return int256(_expectedTotalSupply) == liquiditySum;
    }

    function echidna_check_netFlowRateSumInvariant() public view returns (bool) {
        int96 netFlowRateSum;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            netFlowRateSum += _cfa.getNetFlow(_superToken, address(_testers[i]));
        }
        return netFlowRateSum == 0;
    }

}
