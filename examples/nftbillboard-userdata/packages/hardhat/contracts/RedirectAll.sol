// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import "hardhat/console.sol";

import {
    ISuperfluid,
    ISuperToken,
    ISuperApp,
    ISuperAgreement,
    ContextDefinitions,
    SuperAppDefinitions
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
// When ready to move to leave Remix, change imports to follow this pattern:
// "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {
    CFAv1Library
} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

import {
    SuperAppBase
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";

contract RedirectAll is SuperAppBase {

    ISuperToken private _acceptedToken; // accepted token
    address public _receiver;

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData cfaV1; //initialize cfaV1 variable

    constructor(
        ISuperfluid host,
        ISuperToken acceptedToken,
        address receiver) {
        assert(address(host) != address(0));
        assert(address(acceptedToken) != address(0));
        assert(address(receiver) != address(0));

        _acceptedToken = acceptedToken;
        _receiver = receiver;

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        host.registerApp(configWord);

        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
        host,
        //here, we are deriving the address of the CFA using the host contract
        IConstantFlowAgreementV1(
            address(host.getAgreementClass(
                    keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                ))
            )
        );

    }



    /**************************************************************************
     * Redirect Logic
     *************************************************************************/

    function currentReceiver()
        external view
        returns (
            uint256 startTime,
            address receiver,
            int96 flowRate
        )
    {
        if (_receiver != address(0)) {
            (startTime, flowRate,,) = cfaV1.cfa.getFlow(_acceptedToken, address(this), _receiver);
            receiver = _receiver;
        }
    }

    event ReceiverChanged(address receiver); 

/// @dev If a new stream is opened, or an existing one is opened
    function _updateOutflow(bytes calldata ctx)
        private
        returns (bytes memory newCtx)
    {
      newCtx = ctx;
      // @dev This will give me the new flowRate, as it is called in after callbacks
      int96 netFlowRate = cfaV1.cfa.getNetFlow(_acceptedToken, address(this));
      (,int96 outFlowRate,,) = cfaV1.cfa.getFlow(_acceptedToken, address(this), _receiver);
      int96 inFlowRate = netFlowRate + outFlowRate;
      if (inFlowRate < 0 ) {
          inFlowRate = inFlowRate * -1; // Fixes issue when inFlowRate is negative
      }

      // @dev If inFlowRate === 0, then delete existing flow.
     if (inFlowRate == int96(0)) {
        // @dev if inFlowRate is zero, delete outflow.
        newCtx = cfaV1.deleteFlowWithCtx(newCtx,address(this), _receiver, _acceptedToken);
     }
      else if (outFlowRate != int96(0)){
        newCtx = cfaV1.updateFlowWithCtx(ctx, _receiver, _acceptedToken, inFlowRate);
      } 
    else {
      // @dev If there is no existing outflow, then create new flow to equal inflow
        newCtx = cfaV1.createFlowWithCtx(ctx, _receiver, _acceptedToken, inFlowRate);
      }
    }

    // @dev Change the Receiver of the total flow
    function _changeReceiver( address newReceiver ) internal {
        require(newReceiver != address(0), "New receiver is zero address");
        // @dev because our app is registered as final, we can't take downstream apps
        require(!cfaV1.host.isApp(ISuperApp(newReceiver)), "New receiver can not be a superApp");
        // @dev only engage in flow editting if there is active outflow and transfer is not to self
        (,int96 outFlowRate,,) = cfaV1.cfa.getFlow(_acceptedToken, address(this), _receiver);
        if (newReceiver == _receiver || outFlowRate == 0) return ;
        // @dev delete flow to old receiver
        cfaV1.deleteFlow(address(this), _receiver, _acceptedToken);
        // @dev create flow to new receiver
        cfaV1.createFlow(newReceiver, _acceptedToken, cfaV1.cfa.getNetFlow(_acceptedToken, address(this)));
        // @dev set global receiver to new receiver
        _receiver = newReceiver;

        emit ReceiverChanged(_receiver);
    }

    //public variables which we'll set userData values to
    ISuperfluid.Context public uData;
    string public userData;

    /**************************************************************************
     * SuperApp callbacks
     *************************************************************************/


    function afterAgreementCreated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32, // _agreementId,
        bytes calldata /*_agreementData*/,
        bytes calldata ,// _cbdata,
        bytes calldata _ctx
    )
        external override
        onlyExpected(_superToken, _agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        
        // decode Context - store full context as uData variable for easy visualization purposes
        ISuperfluid.Context memory decompiledContext = cfaV1.host.decodeCtx(_ctx);
        uData = decompiledContext;

        //set userData variable to decoded value
        //for now, this value is hardcoded as a string - this will be made clear in flow creation scripts within the tutorial
        //this string will serve as a message on an 'NFT billboard' when a flow is created with recipient = tradeableCashflow
        //it will be displayed on a front end for assistance in userData explanation
        userData = abi.decode(decompiledContext.userData, (string));
        
        return _updateOutflow(_ctx);
    }

    function afterAgreementUpdated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 ,//_agreementId,
        bytes calldata /*_agreementData*/,
        bytes calldata ,//_cbdata,
        bytes calldata _ctx
    )
        external override
        onlyExpected(_superToken, _agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        ISuperfluid.Context memory decompiledContext = cfaV1.host.decodeCtx(_ctx);
        uData = decompiledContext;

        userData = abi.decode(decompiledContext.userData, (string));

        return _updateOutflow(_ctx);
    }

    function afterAgreementTerminated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 ,//_agreementId,
        bytes calldata, // _agreementData,
        bytes calldata ,//_cbdata,
        bytes calldata _ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        // According to the app basic law, we should never revert in a termination callback
        if (!_isSameToken(_superToken) || !_isCFAv1(_agreementClass)) return _ctx;
        userData = "";
        return _updateOutflow(_ctx);
    }

    function _isSameToken(ISuperToken superToken) private view returns (bool) {
        return address(superToken) == address(_acceptedToken);
    }

    function _isCFAv1(address agreementClass) private view returns (bool) {
        return ISuperAgreement(agreementClass).agreementType()
            == keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

    modifier onlyHost() {
        require(msg.sender == address(cfaV1.host), "RedirectAll: support only one host");
        _;
    }

    modifier onlyExpected(ISuperToken superToken, address agreementClass) {
        require(_isSameToken(superToken), "RedirectAll: not accepted token");
        require(_isCFAv1(agreementClass), "RedirectAll: only CFAv1 supported");
        _;
    }
}
