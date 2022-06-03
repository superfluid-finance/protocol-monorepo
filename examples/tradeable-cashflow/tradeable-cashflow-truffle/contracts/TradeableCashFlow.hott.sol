// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import "@superfluid-finance/hot-fuzz/contracts/HotFuzzBase.sol";

import {TradeableCashflow} from "./TradeableCashflow.sol";


contract NFTLunatic is SuperfluidTester {
    TradeableCashflow private _app;

    constructor (
        SuperfluidFrameworkDeployer.Framework memory sf,
        IERC20 token,
        ISuperToken superToken)
        SuperfluidTester(sf, token, superToken)
    {
    }

    function setApp(TradeableCashflow app) external {
        _app = app;
    }

    function apeInOrChickenOut(int96 flowRate) external {
        flow(address(_app), flowRate);
    }

    function dumpIt(NFTLunatic maybeNotMe) external {
        try _app.transferFrom(address(this), address(maybeNotMe), 1) {
        } catch {
            assert(false);
        }
    }
}

contract TradeableCashFlowHotFuzz is HotFuzzBase {

    TradeableCashflow immutable private _app;
    NFTLunatic private _currentBagHolder;

    constructor() HotFuzzBase(10 /* nTesters */) {
        initTesters();
        // ... setup your app
        _currentBagHolder = NFTLunatic(address(testers[0]));
        _app = new TradeableCashflow(
            address(_currentBagHolder),
            "LUNA NFT", "LUNA", sf.host, superToken
        );
        for (uint i = 0; i < nTesters; ++i) {
            NFTLunatic(address(testers[i])).setApp(_app);
        }
        // to take app balance into account during basic invariances checks
        addAccount(address(_app));
    }

    // hot fuzz extensions
    function createTester() override internal returns (SuperfluidTester) {
        return new NFTLunatic(sf, token, superToken);
    }

    function getOneHodler(uint8 a)
        internal view
        returns (NFTLunatic testerA)
    {
        return NFTLunatic(address(getOneTester(a)));
    }

    /**************************************************************************
    * Tester Actions
    **************************************************************************/

    function someLunaticApeInOrChickenOut(uint8 a, int64 flowRate) external {
        NFTLunatic hodler = getOneHodler(a);

        hodler.apeInOrChickenOut(flowRate);
    }

    function theHodlerDumped(uint8 a) external {
        NFTLunatic newBagHolder = getOneHodler(a);
        _currentBagHolder.dumpIt(newBagHolder);
        _currentBagHolder = newBagHolder;
    }

    /**************************************************************************
    * Invariances
    **************************************************************************/

    function echidna_app_is_free() public view returns (bool) {
        return sf.host.isApp(_app) && !sf.host.isAppJailed(_app);
    }

    function echidna_app_no_rug_you() public view returns (bool) {
        return sf.cfa.getNetFlow(superToken, address(_app)) == 0;
    }

    function echidna_hodler_be_whale() public view returns (bool) {
        int96 totalInputFlowRate;
        for (uint i = 0; i < nTesters; ++i) {
            NFTLunatic hodler = NFTLunatic(address(testers[i]));
            (,int96 r,,) = sf.cfa.getFlow(superToken, address(hodler), address(_app));
            totalInputFlowRate += r;
        }
        (,int96 hodlerApingFlowRate,,) = sf.cfa.getFlow(superToken, address(_currentBagHolder), address(_app));
        (,int96 appPayingFlowRate,,) = sf.cfa.getFlow(superToken, address(_app), address(_currentBagHolder));
        int96 hodlerNetFlowRate = sf.cfa.getNetFlow(superToken, address(_currentBagHolder));
        return
            hodlerNetFlowRate == totalInputFlowRate - hodlerApingFlowRate &&
            appPayingFlowRate == totalInputFlowRate;
    }

}
