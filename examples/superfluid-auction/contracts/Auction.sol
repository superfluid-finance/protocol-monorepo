// SPDX-License-Identifier: MIT
pragma solidity ^0.7.4;
pragma abicoder v2;


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
    SuperAppBase
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";

contract Auction is SuperAppBase {

    ISuperfluid private host;
    IConstantFlowAgreementV1 private cfa;
    ISuperToken private superToken;
    int96 public minStep = 1;
    int96 private markup;
    int96 private precision = 100000; // this is the maximum precision of the minStep
    mapping(address => mapping(bytes32 => address)) private _userAgreements;

    struct Bid{
  		int96 flowRate;
      address next;
      address prev;
    }
    mapping(address => Bid) public bidders; // ordered array of all bids.
    // simple way to avoid having to sort the bids is that you only allow high bids
    address public winner = address(this);  // current winner

    constructor(
        ISuperfluid _host,
        IConstantFlowAgreementV1 _cfa,
        ISuperToken _superToken,
        int96 _markup // percentage with 3 decimals ex: 133333 =  133.333%
    ) {
        require(address(_host) != address(0), "host is nil");
        require(address(_cfa) != address(0), "cfa is nil");
        require(address(_superToken) != address(0), "superToken1 is nil");

        host = _host;
        cfa = _cfa;
        superToken = _superToken;
        markup = _markup;

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        host.registerApp(configWord);
    }

    function _placeBid(bytes calldata _ctx, bytes32 agreementId)
        private
        returns (bytes memory newCtx)
    {
        newCtx = _ctx;
        address user = host.decodeCtx(_ctx).msgSender;
        (,int96 flowRate,,) = cfa.getFlowByID(superToken, agreementId);
        require(flowRate * precision >= minStep, "Bid must be higher than minimum step");
        if(winner == address(this)) _setNewWinner(user, flowRate);
        else if(flowRate * precision >= bidders[winner].flowRate * precision + minStep){  //if flowRate > winnerFlowRate, but not by minStep, still goes in second place
            newCtx = _cancelBack(newCtx,  winner);
            _setNewWinner(user, flowRate);
        } else {
            _placeInList(user, flowRate);
            newCtx = _cancelBack(newCtx,  user);
        }
    }

    function _updateBid(bytes calldata _ctx, bytes32 agreementId)  // this is likely gonna be complicated
        private
        returns (bytes memory newCtx)
    {
        address user = host.decodeCtx(_ctx).msgSender;
        (,int96 flowRate,,) = cfa.getFlowByID(superToken,agreementId);
        require(flowRate * precision >= minStep, "Bid must be higher than minimum step");
        newCtx = _ctx;

        if(user == winner) {   //I've seen people do keccak256 here not sure if I need to.
            int96 altMinStep = bidders[bidders[user].next].flowRate * (markup - precision);
            //require(flowRate * precision >= altMinStep, "Bid must be higher than minimum step");
            if(flowRate * precision > bidders[bidders[user].next].flowRate * precision + altMinStep) {
                // user is still the winner. adjust minStep, nothing else changes.
                bidders[user].flowRate = flowRate;
                minStep = flowRate * (markup - precision);
            } else {
                // user is not winner anymore.  sort out new winner
                minStep = altMinStep;
                winner = bidders[user].next;
                bidders[winner].prev = address(this);
                newCtx = _stopCancelBack(newCtx, winner);
                // find a place for old winner in list
                _placeInList(user, flowRate);
                newCtx = _cancelBack(newCtx, user);
            }
        } else {
            // unlink it from the list
            bidders[bidders[user].prev].next = bidders[user].next;
            bidders[bidders[user].next].prev = bidders[user].prev;
            // check if he should be winner
            if(flowRate * precision >= bidders[winner].flowRate * precision + minStep){  //if flowRate > winnerFlowRate, but not by minStep, still goes in second place. WEIRD
                newCtx = _stopCancelBack(newCtx, user);
                newCtx = _cancelBack(newCtx,  winner);
                _setNewWinner(user, flowRate);
            } else{
                // place it in the list again
                _placeInList(user,flowRate);
                // adjust _cancelBack amount
                newCtx = _updateCancelBack(newCtx,  user); // have to do this in all cases I think
            }
        }
    }

    function _setNewWinner(address bidder, int96 flowRate) private {   //   use this only if new winner is result of bid increased over current winner
        bidders[bidder] = Bid(flowRate, winner, address(this));
        bidders[winner].prev = bidder;
        winner = bidder;
        minStep = flowRate * (markup - precision);
    }

    function _cancelBack(bytes memory ctx, address bidder)
        private
        returns (bytes memory newCtx)
    {
          (newCtx, ) = host.callAgreementWithContext(
              cfa,
              abi.encodeWithSelector(
                  cfa.createFlow.selector,
                  superToken,
                  bidder,
                  bidders[bidder].flowRate,
                  new bytes(0) // placeholder
              ),
              "0x",
              ctx
          );
    }

    function _updateCancelBack(bytes memory ctx, address bidder)
        private
        returns (bytes memory newCtx)
    {
          (newCtx, ) = host.callAgreementWithContext(
              cfa,
              abi.encodeWithSelector(
                  cfa.updateFlow.selector,
                  superToken,
                  bidder,
                  bidders[bidder].flowRate,
                  new bytes(0) // placeholder
              ),
              "0x",
              ctx
          );
    }

    function _stopCancelBack(bytes memory _ctx, address bidder)
        private
        returns (bytes memory newCtx)
    {
        if(bidder == address(0)) return newCtx = _ctx;
        (newCtx, ) = host.callAgreementWithContext(
            cfa,
            abi.encodeWithSelector(
                cfa.deleteFlow.selector,
                superToken,
                address(this),
                bidder,
                new bytes(0)
            ),
            "0x",
            _ctx
        );
    }

    function _placeInList(address user, int96 flowRate)
        private
    {
        address tempNext = winner;
        address tempPrev;
        do {
            tempPrev = tempNext;
            tempNext = bidders[tempPrev].next;
        } while (flowRate * precision < bidders[tempNext].flowRate * precision + minStep);
        bidders[user] = Bid(flowRate, tempNext, tempPrev);
        bidders[tempPrev].next = user;
        bidders[tempNext].prev = user;
    }

    function _closeBid(bytes calldata _ctx, bytes32 agreementId, address user)
        private
        returns (bytes memory newCtx)
    {
      // if(user != (host.decodeCtx(_ctx)).msgSender) then user_was_liquidated;
      // could check if the agreementId is derived by user+app.address

      if(address(user) == address(winner)){
          winner = bidders[user].next;
          bidders[winner].prev = address(this); // not sure it's relevant actually
          if(winner != address(this)){
              newCtx = _stopCancelBack(_ctx, winner);
              minStep = bidders[winner].flowRate * (markup - precision);
          }
      } else { // address is rando non-winner in the list
          newCtx = _stopCancelBack(_ctx, user);
          bidders[bidders[user].prev].next = bidders[user].next;
          bidders[bidders[user].next].prev = bidders[user].prev;
      }
      delete bidders[user];
    }
    /**************************************************************************
     * SuperApp callbacks
     *************************************************************************/
    function afterAgreementCreated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 _agreementId,
        bytes calldata /*_agreementData*/,
        bytes calldata _cbdata,
        bytes calldata _ctx
    )
        external override
        onlyExpected(_superToken, _agreementClass)
        onlyHost
        returns (bytes memory)
    {
        return _placeBid(_ctx, _agreementId);
    }

    function afterAgreementUpdated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 _agreementId,
        bytes calldata /*_agreementData*/,
        bytes calldata /*_cbdata*/,
        bytes calldata _ctx
    )
        external override
        onlyExpected(_superToken, _agreementClass)
        onlyHost
        returns (bytes memory)
    {
        return _updateBid(_ctx, _agreementId); //this should break instead
    }

    function afterAgreementTerminated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 _agreementId,
        bytes calldata _agreementData,
        bytes calldata /*_cbdata*/,
        bytes calldata _ctx
    )
        external override
        onlyHost
        returns (bytes memory)
    {
        // According to the app basic law, we should never revert in a termination callback
        if (!_isAccepted(_superToken) || !_isCFAv1(_agreementClass)) return _ctx;
        (address user,) = abi.decode(_agreementData, (address, address));
        return _closeBid(_ctx, _agreementId, user);
    }

    // utilities
    function _isAccepted(ISuperToken _superToken) private view returns (bool) {
        return address(_superToken) == address(superToken);
    }

    function _isCFAv1(address _agreementClass) private view returns (bool) {
        return ISuperAgreement(_agreementClass).agreementType()
            == keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

    modifier onlyHost() {
        require(msg.sender == address(host), "RedirectAll: support only one host");
        _;
    }

    modifier onlyExpected(ISuperToken _superToken, address _agreementClass) {
        require(_isAccepted(_superToken) , "Auction: not accepted tokens");
        require(_isCFAv1(_agreementClass), "Auction: only CFAv1 supported");
        _;
    }

}
