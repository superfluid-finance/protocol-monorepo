// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../superfluid/Superfluid.sol";
import "../superfluid/SuperToken.sol";
import "../test/TestGovernance.sol";
import "../agreements/ConstantFlowAgreementV1.sol";
import "../agreements/InstantDistributionAgreementV1.sol";
import "../apps/CFAv1Library.sol";
import "../apps/IDAv1Library.sol";
import "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";

contract SuperfluidTester {

    CFAv1Library.InitData private _cfaLib;
    IDAv1Library.InitData private _idaLib;
    IERC20 private _token;
    ISuperToken private _superToken;

    using CFAv1Library for CFAv1Library.InitData;

    constructor (
        Superfluid host,
        IConstantFlowAgreementV1 cfa,
        IERC20 token,
        ISuperToken superToken) {
        _cfaLib.host = host;
        _cfaLib.cfa = cfa;
        _idaLib.host = host;
        _idaLib.cfa = cfa;
        _token = token;
        _superToken = superToken;
    }

    function upgradeSuperToken(uint256 amount) external {
        _token.approve(address(_superToken), amount);
        _superToken.upgrade(amount);
    }

    function downgradeSuperToken(uint256 amount) external {
        _superToken.downgrade(amount);
    }

    function flow(address receiver, int96 flowRate) external {
        _cfaLib.flow(receiver, _superToken, flowRate);
    }

}

contract NoCallbackSuperfluidFuzzer {

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

        _host = new Superfluid(true /* nonUpgradable */, false /* appWhiteListingEnabled */);

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
            _testers[i] = new SuperfluidTester(_host, _cfa, _token, _superToken);
            _token.mint(address(_testers[i]), INIT_TOKEN_BALANCE);
            _testers[i].upgradeSuperToken(INIT_SUPER_TOKEN_BALANCE);
        }
    }

    function _getTester1(uint8 a)
        private view
        returns (SuperfluidTester testerA) {
        testerA = _testers[a % N_TEST_ACCOUNTS];
    }

    function _getTester2(uint8 a, uint8 b)
        private view
        returns (SuperfluidTester testerA, SuperfluidTester testerB) {
        testerA = _testers[a % N_TEST_ACCOUNTS];
        testerB = _testers[((a % N_TEST_ACCOUNTS) + (b % (N_TEST_ACCOUNTS - 1))) % N_TEST_ACCOUNTS];
    }

    function upgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = _getTester1(a);

        uint256 a1 = _superToken.balanceOf(address(tester));
        uint256 b1 = _token.balanceOf(address(tester));
        tester.upgradeSuperToken(amount);
        uint256 a2 = _superToken.balanceOf(address(tester));
        uint256 b2 = _token.balanceOf(address(tester));
        assert(amount == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        _expectedTotalSupply += amount;
    }

    function downgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = _getTester1(a);

        uint256 a1 = _superToken.balanceOf(address(tester));
        uint256 b1 = _token.balanceOf(address(tester));
        tester.downgradeSuperToken(amount);
        uint256 a2 = _superToken.balanceOf(address(tester));
        uint256 b2 = _token.balanceOf(address(tester));
        assert(amount == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        _expectedTotalSupply -= amount;
    }

    function createFlow(uint8 a, uint8 b, uint32 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTester2(a, b);

        testerA.flow(address(testerB), int96(uint96(flowRate)));
    }

    function deleteFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTester2(a, b);

        testerA.flow(address(testerB), 0);
    }

    function totalSupplyInvariant() public {
        assert(_superToken.totalSupply() == _expectedTotalSupply);
    }

    function liquiditySumInvanriant() public {
        int256 liquiditySum;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            (int256 avb, uint256 d, uint256 od,) = _superToken.realtimeBalanceOfNow(address(_testers[i]));
            liquiditySum += avb + int256(d) - int256(od);
        }
        assert(int256(_expectedTotalSupply) == liquiditySum);
    }

    function netFlowRateSumInvanriant() public {
        int96 netFlowRateSum;
        for (uint i = 0; i < N_TEST_ACCOUNTS; ++i) {
            netFlowRateSum += _cfa.getNetFlow(_superToken, address(_testers[i]));
        }
        assert(netFlowRateSum == 0);
    }

}
