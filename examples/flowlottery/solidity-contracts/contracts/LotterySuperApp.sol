// SPDX-License-Identifier: MIT
// solhint-disable not-rely-on-time
pragma solidity 0.8.13;

import {
    ISuperfluid,
    ISuperToken,
    ISuperAgreement,
    SuperAppDefinitions
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {
    CFAv1Library
} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

import {
    SuperAppBase
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";


import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";


contract LotterySuperApp is Ownable, SuperAppBase {

    /// @dev Entrance fee for the game (hardcoded to $1)
    uint256 constant public ENTRANCE_FEE = 1e18;
    /// @dev Minimum flow rate to participate (hardcoded to $10 / mo)
    int96 constant public MINIMUM_FLOW_RATE = int96(int256(uint256(10e18) / uint256(3600 * 24 * 30)));

    string constant private _ERR_STR_NO_TICKET = "LotterySuperApp: need ticket to play";
    string constant private _ERR_STR_LOW_FLOW_RATE = "LotterySuperApp: flow rate too low";

    ISuperfluid private _host; // host
    IConstantFlowAgreementV1 private _cfa; // the stored constant flow agreement class address
    ISuperToken private _acceptedToken; // accepted token

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1; //initialize cfaV1 variable

    EnumerableSet.AddressSet private _playersSet;
    using EnumerableSet for EnumerableSet.AddressSet;
    address private _winner;

    constructor(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperToken acceptedToken) {
        assert(address(host) != address(0));
        assert(address(cfa) != address(0));
        assert(address(acceptedToken) != address(0));

        _host = host;
        _cfa = cfa;
        _acceptedToken = acceptedToken;

        // NOTE: this may be incorrect
        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL;

        _host.registerApp(configWord);

        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
            host,
            //here, we are deriving the address of the CFA using the host contract
            IConstantFlowAgreementV1(address(host.getAgreementClass(
                    keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
            )))
        );

    }

    /// @dev Tickets by users
    mapping (address => uint) public tickets;

    /**************************************************************************
     * Game Logic
     *************************************************************************/

    /// @dev Take entrance fee from the user and issue a ticket
    function participate(bytes calldata ctx)
        external
        onlyHost
        returns (bytes memory newCtx)
    {
        // msg sender is encoded in the Context
        address sender = _host.decodeCtx(ctx).msgSender;
        _acceptedToken.transferFrom(sender, address(this), ENTRANCE_FEE);
        tickets[sender]++;
        return ctx;
    }

    function currentWinner()
        external view
        returns (
            uint256 drawingTime,
            address player,
            int96 flowRate
        )
    {
        if (_winner != address(0)) {
            (drawingTime, flowRate,,) = _cfa.getFlow(_acceptedToken, address(this), _winner);
            player = _winner;
        }
    }

    event WinnerChanged(address winner);

    /// @dev Check requirements before letting the user playing the game
    function _beforePlay(
        bytes calldata ctx
    )
        private view
        returns (bytes memory cbdata)
    {
        address sender = _host.decodeCtx(ctx).msgSender;
        require(tickets[sender] > 0, _ERR_STR_NO_TICKET);
        cbdata = abi.encode(sender);
    }

    /// @dev Play the game
    function _play(
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        private
        returns (bytes memory newCtx)
    {
        (address player) = abi.decode(cbdata, (address));

        (,int96 flowRate,,) = IConstantFlowAgreementV1(agreementClass).getFlowByID(_acceptedToken, agreementId);
        require(flowRate >= MINIMUM_FLOW_RATE, _ERR_STR_LOW_FLOW_RATE);

        _playersSet.add(player);

        // charge one ticket
        tickets[player]--;

        return _draw(player, ctx);
    }

    /// @dev Play the game
    function _quit(
        address player,
        bytes calldata ctx
    )
        private
        returns (bytes memory newCtx)
    {
        _playersSet.remove(player);

        return _draw(player, ctx);
    }

    // @dev Make a new draw
    function _draw(
        address player,
        bytes calldata ctx
    )
        private
        returns (bytes memory newCtx)
    {
        address oldWinner = _winner;

        if (_playersSet.length() > 0) {
            // use block hash
            _winner = _playersSet.at(
                // not the most perfect randomness source
                // DO NOT USE blockhash(block.number) though
                uint(keccak256(abi.encodePacked(
                    player,
                    _playersSet.length(),
                    blockhash(block.number - 1),
                    block.timestamp // this is where could be manipulated by miner...
                )))
                %
                _playersSet.length()
            );
        } else {
            _winner = address(0);
        }

        newCtx = ctx;

        // delete flow to old winner
        if (oldWinner != address(0)) {
            newCtx = cfaV1.deleteFlowWithCtx(
                newCtx,
                address(this),
                oldWinner,
                _acceptedToken);
        }

        // create flow to new winner
        if (_winner != address(0)) {
            newCtx = cfaV1.createFlowWithCtx(newCtx,
                _winner,
                _acceptedToken,
                _cfa.getNetFlow(_acceptedToken, address(this))
                );
        }

        emit WinnerChanged(_winner);
    }

    /**************************************************************************
     * SuperApp callbacks
     *************************************************************************/

    function beforeAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view override
        onlyHost
        onlyExpected(superToken, agreementClass)
        returns (bytes memory cbdata)
    {
        cbdata = _beforePlay(ctx);
    }

    function afterAgreementCreated(
        ISuperToken /* superToken */,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*agreementData*/,
        bytes calldata cbdata,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        return _play(ctx, agreementClass, agreementId, cbdata);
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view override
        onlyHost
        onlyExpected(superToken, agreementClass)
        returns (bytes memory cbdata)
    {
        cbdata = _beforePlay(ctx);
    }

    function afterAgreementUpdated(
        ISuperToken /* superToken */,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*agreementData*/,
        bytes calldata cbdata,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        return _play(ctx, agreementClass, agreementId, cbdata);
    }

    function beforeAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*ctx*/
    )
        external view override
        onlyHost
        returns (bytes memory cbdata)
    {
        // According to the app basic law, we should never revert in a termination callback
        if (!_isSameToken(superToken) || !_isCFAv1(agreementClass)) return abi.encode(true);
        return abi.encode(false);
    }

    ///
    function afterAgreementTerminated(
        ISuperToken /* superToken */,
        address /* agreementClass */,
        bytes32 /* agreementId */,
        bytes calldata agreementData,
        bytes calldata cbdata,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        // According to the app basic law, we should never revert in a termination callback
        (bool shouldIgnore) = abi.decode(cbdata, (bool));
        if (shouldIgnore) return ctx;
        // note that msgSender can be either flow sender, receiver or liquidator
        // one must decode agreementData to determine who is the actual player
        (address player, ) = abi.decode(agreementData, (address, address));
        return _quit(player, ctx);
    }

    function _isSameToken(ISuperToken superToken) private view returns (bool) {
        return address(superToken) == address(_acceptedToken);
    }

    function _isCFAv1(address agreementClass) private view returns (bool) {
        return ISuperAgreement(agreementClass).agreementType()
            == keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

    modifier onlyHost() {
        require(msg.sender == address(_host), "LotterySuperApp: support only one host");
        _;
    }

    modifier onlyExpected(ISuperToken superToken, address agreementClass) {
        require(_isSameToken(superToken), "LotterySuperApp: not accepted token");
        require(_isCFAv1(agreementClass), "LotterySuperApp: only CFAv1 supported");
        _;
    }

}
