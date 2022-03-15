// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "../superfluid/Superfluid.sol";
import "../superfluid/SuperToken.sol";
import "../agreements/ConstantFlowAgreementV1.sol";
import "../agreements/InstantDistributionAgreementV1.sol";
import "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";

contract NoCallbackSuperfluidFuzzer {

    uint private constant INIT_BALANCE = type(uint128).max;
    Superfluid private immutable _sf;
    ConstantFlowAgreementV1 private immutable _cfa;
    ERC20PresetMinterPauser private immutable _token;
    ISuperToken private immutable _superToken;

    // test state
    uint256 expectedTotalSupply;

    constructor() {
        _sf = new Superfluid(true /* nonUpgradable */, false /* appWhiteListingEnabled */);
        _sf.initialize(ISuperfluidGovernance(address(this)));

        _cfa = new ConstantFlowAgreementV1(_sf);
        _sf.registerAgreementClass(_cfa);

        _token = new ERC20PresetMinterPauser("FTT", "FTT");
        _token.mint(address(this), INIT_BALANCE);
        _token.mint(address(0x100), INIT_BALANCE);
        _token.mint(address(0x200), INIT_BALANCE);
        _token.mint(address(0x300), INIT_BALANCE);
        _token.mint(address(0x400), INIT_BALANCE);
        _token.mint(address(0x500), INIT_BALANCE);

        _superToken = new SuperToken(_sf);
        _superToken.initialize(
            _token,
            18,
            "FTTx",
            "FTTx");
    }

    function upgrade(uint64 amount) public {
        require(amount > 0);
        uint256 a1 = _superToken.balanceOf(msg.sender);
        uint256 b1 = _token.balanceOf(address(this));
        _token.approve(address(_superToken), amount);
        _superToken.upgradeTo(msg.sender, amount, "");
        uint256 a2 = _superToken.balanceOf(msg.sender);
        uint256 b2 = _token.balanceOf(address(this));
        assert(amount == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        expectedTotalSupply += amount;
    }

    function upgrade(uint32 flowRate) public {
        require(amount > 0);
        //_sf.callAgreement
    }

    /* function downgrade(uint64 amount) public {
        require(amount > 0);
        uint256 a1 = _superToken.balanceOf(msg.sender);
        uint256 b1 = _token.balanceOf(msg.sender);
        _superToken.downgrade(amount);
        uint256 a2 = _superToken.balanceOf(msg.sender);
        uint256 b2 = _token.balanceOf(msg.sender);
        assert(amount == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        expectedTotalSupply -= amount;
    } */

    function totalSupplyInvariant() public {
        assert(_superToken.totalSupply() == expectedTotalSupply);
    }

}
