// SPDX-License-Identifier: MIT
// solhint-disable not-rely-on-time
pragma solidity 0.8.13;

import "@superfluid-finance/hot-fuzz/contracts/HotFuzzBase.sol";

import "./LotterySuperApp.sol";

contract LotteryPlayer is SuperfluidTester {

    LotterySuperApp immutable private _app;
    bool public inPlay;

    constructor (
        SuperfluidFrameworkDeployer.Framework memory sf,
        IERC20 token,
        ISuperToken superToken,
        LotterySuperApp app)
        SuperfluidTester(sf, token, superToken)
    {
        _app = app;
    }

    function play(int96 flowRate) external {
        superToken.approve(address(_app), _app.ENTRANCE_FEE());
        sf.host.callAppAction(
            _app,
            abi.encodeCall(_app.participate, (new bytes(0)))
        );
        flow(address(_app), flowRate);
        inPlay = true;
    }

    function quitIfNeeded() external {
        flow(address(_app), 0);
        inPlay = false;
    }
}

contract LotterySuperAppHotFuzz is HotFuzzBase {

    LotterySuperApp immutable private _app;

    constructor() HotFuzzBase(10 /* nPlayers */ ) {
        _app = new LotterySuperApp(sf.host, sf.cfa, superToken);
        initTesters();
        addAccount(address(_app));
    }

    // hot fuzz extensions
    function createTester() override internal returns (SuperfluidTester) {
        return new LotteryPlayer(sf, token, superToken, _app);
    }

    function getOnePlayer(uint8 a)
        internal view
        returns (LotteryPlayer testerA)
    {
        return LotteryPlayer(address(getOneTester(a)));
    }

    //
    // Actions List
    //
    function participateLottery(uint8 a, int64 flowRate) public {
        LotteryPlayer player = getOnePlayer(a);
        require(flowRate >= _app.MINIMUM_FLOW_RATE());

        player.play(flowRate);
    }

    function leaveLottery(uint8 a) public {
        LotteryPlayer player = getOnePlayer(a);

        player.quitIfNeeded();
    }

    //
    // Invariances
    //
    function echidna_app_is_free() public view returns (bool) {
        return sf.host.isApp(_app) && !sf.host.isAppJailed(_app);
    }

    function echidna_always_has_a_winner() public view returns (bool) {
        bool someoneInPlay = false;
        for (uint i = 0; i < nTesters; ++i) {
            LotteryPlayer player = LotteryPlayer(address(testers[i]));
            if (player.inPlay()) {
                someoneInPlay = true;
                break;
            }
        }
        (,address winner,) = _app.currentWinner();
        if (someoneInPlay) return winner != address(0);
        else return winner == address(0);
    }

    //event DEBUG_WINNER_TAKE_ALL(int96 winnerNetFlowRate, int96 totalInputFlowRate, int96 winerPlayingFlowRate);
    function echidna_winner_takes_all() public view returns (bool) {
        int96 totalInputFlowRate;
        for (uint i = 0; i < nTesters; ++i) {
            LotteryPlayer player = LotteryPlayer(address(testers[i]));
            (,int96 r,,) = sf.cfa.getFlow(superToken, address(player), address(_app));
            totalInputFlowRate += r;
        }
        (,address winner,) = _app.currentWinner();
        (,int96 winnerPlayingFlowRate,,) = sf.cfa.getFlow(superToken, address(winner), address(_app));
        int96 winnerNetFlowRate = sf.cfa.getNetFlow(superToken, address(winner));
        //emit DEBUG_WINNER_TAKE_ALL(winnerNetFlowRate, totalInputFlowRate, winerPlayingFlowRate);
        return winnerNetFlowRate == totalInputFlowRate - winnerPlayingFlowRate;
    }
}
