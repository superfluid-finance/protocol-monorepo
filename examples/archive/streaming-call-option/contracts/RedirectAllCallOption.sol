// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import "@chainlink/contracts/src/v0.8/interfaces/AggregatorV3Interface.sol";

import {ERC20} from "@openzeppelin/contracts/token/ERC20/ERC20.sol";


import {
    ISuperfluid,
    ISuperToken,
    ISuperApp,
    ISuperAgreement,
    ContextDefinitions,
    SuperAppDefinitions
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

// https://github.com/superfluid-finance/protocol-monorepo/blob/remix-support/packages/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

// When ready to move to leave Remix, change imports to follow this pattern:
// 
import {
    IConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {
    CFAv1Library
} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

import {
    SuperAppBase
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";

contract RedirectAllCallOption is SuperAppBase {
    
    //underlyingAsset for the option 
    ERC20 public _underlyingAsset;
    //the amount of the underlyingAsset deposited
    uint256 public _underlyingAmount;
    //decimals in underlyingAsset
    uint8 public _underlyingDecimals;
    
    //asset that can be used to purchase _underlyingAsset
    //this needs to be hardcoded to rinkeby DAI because we will assume that price feeds will be denominated in USD on chainlink
    //you could probably get more advanced with this, but trying to keep it simple
    ERC20 public _dai;
    
    //address of the price feed used for the _underlyingAsset
    AggregatorV3Interface public _priceFeed;

    //decimals returned by the priceFeed
    uint8 public _priceFeedDecimals;
    //strike price of the option. The option may be exercised if the current price is > that _strikePrice prior to the expirationDate & flowRate into the app is > required rate
    int256 public _strikePrice;
    //required flow rate to exercise the option
    int96 public _requiredFlowRate;
    //expiration date of the option (timestamp)
    uint256 public _expirationDate;
    
    //sets whether or not the option can be activated 
    bool public optionReady;
    //sets whether or not the option is active 
    bool public optionActive;
    
    //superfluid params
    ISuperfluid private _host; // host
    IConstantFlowAgreementV1 private _cfa; // the stored constant flow agreement class address

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData cfaV1; //initialize cfaV1 variable
    
    //the Super token used to pay for option premium (sent directly to the NFT and redirected to owner of the NFT)
    ISuperToken public _acceptedToken; // accepted token
    //receiver of this NFT - the seller of the option 
    address public _receiver;

    constructor(
        
        //set superfluid specific params, receiver, and accepted token in the constructor
        
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        ISuperToken acceptedToken,
        ERC20 dai,
        address receiver) {
        require(address(host) != address(0));
        require(address(cfa) != address(0));
        require(address(acceptedToken) != address(0));
        require(address(receiver) != address(0));
        require(!host.isApp(ISuperApp(receiver)));

        _host = host;
        _cfa = cfa;
        _acceptedToken = acceptedToken;
        _dai = dai;
        _receiver = receiver;

        //sets the optionReady param as false to start until the _recevier creates the option 
        optionReady = false; 
        //sets optionActive to be false to start because the option has not been activated 
        optionActive = false;

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);

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
     * Option Logic
     *************************************************************************/
    
    //option creation is split into 2 functions: setOptionParams and activateOption...
    //the first of which handles the approval, and the second of which transfers funds + activates
    
    //this function sets the params of the option and sets it as ready to be activated
    //need to find a way to properly convert decimals 
    //we have decimals of returned value of price feed, decimals of underlying, and decimals on DAI
    //DAI is hardcoded in this example, so don't have to worry about that for now
    //we need to align the decimal value of price feed return and underlying asset
    function createOption(
            ERC20 underlyingAsset,
            uint256 underlyingAmount, 
            uint8 underlyingDecimals,
            AggregatorV3Interface priceFeed, 
            uint8 priceFeedDecimals,
            int96 requiredFlowRate, 
            uint256 expirationDate, 
            int256 strikePrice) external {
    
        //only the receiver (the seller of the option and owner of the NFT) can activate the option 
        require(_receiver == msg.sender);
        _underlyingAsset = ERC20(underlyingAsset);
        _underlyingAmount = underlyingAmount;
        _underlyingDecimals = underlyingDecimals;
        _priceFeed = priceFeed;
        _priceFeedDecimals = priceFeedDecimals;
        _strikePrice = strikePrice;
        _requiredFlowRate = requiredFlowRate;
        _expirationDate = expirationDate;
        
        optionReady = true;
    }
    
    //activates the option - called in callbacks
    function _activateOption() internal {
        //send underlying assets to the contract 
        //Receiver MUST approve contract to spend funds before the option is activated - would be another flow on front end
        _underlyingAsset.transferFrom(_receiver, address(this), _underlyingAmount);
        //set option as active 
        optionActive = true;
        //consider adding additional functionality around expirationDate here as well
    }
    
    
    //deactivate the option - this is called when the option needs to be cancelled
    function _deactivateOption() internal {
        
        //send locked funds back to the owner of the NFT 
        _underlyingAsset.transfer(_receiver, _underlyingAmount);
        //change expiration date to now so that option can no longer be exercised 
        _expirationDate = block.timestamp;
        
        //set option to inactive & not ready
        optionActive = false;
        optionReady = false;
        
        //as of now, it will not cancel flows into the contract
    }
    
    //enables the caller to exercise the option 
    function exerciseOption() external {
        require(_dai.allowance(msg.sender, address(this)) >= uint(_strikePrice), "must call approve first");
        //start function by checking if option is expired - if it is, we need to deactivate the option
         if (block.timestamp >= _expirationDate) {
            //transfer underlying asset back to owner and cancel flows 
            _deactivateOption();
        }
        else {
        //exercise the option        
        //get current price of the underlying asset using chainlink 
        (, int currentPrice,,,) = _priceFeed.latestRoundData();

        //adjust current price if the price feed decimals and underlying decimals are different
        //this is important for comparison between current price and strike price

        if (_priceFeedDecimals != _underlyingDecimals) {
            int256 _adjustedDecimals = int256(uint256(_underlyingDecimals - _priceFeedDecimals));

            if (_adjustedDecimals < 0) {
                //if _adjusted decimals is negative, adjust so that it's positive
                _adjustedDecimals = _adjustedDecimals * -1;
            }
            currentPrice = int(uint(currentPrice) * (10 ** uint(_adjustedDecimals)));
        }

        //get flow rate of the flow that the caller of this function is sending to the contract 
        (, int96 currentFlowRate,,) = _cfa.getFlow(_acceptedToken, msg.sender, address(this));
        //require that flowRate is sufficient & option is in the money for option to be exercises
        require(currentPrice >= _strikePrice, "option out of the money");
        require(currentFlowRate >= _requiredFlowRate, "insufficient flow rate");
        
    
        //NOTE - caller of this function MUST approve DAI first before calling this function
        _dai.transferFrom(msg.sender, _receiver, uint256(_strikePrice));
        _underlyingAsset.transfer(msg.sender, _underlyingAmount);
        
        cfaV1.deleteFlow(address(this), _receiver, _acceptedToken);

        cfaV1.deleteFlow(msg.sender, address(this), _acceptedToken);

        optionActive = false;
        optionReady = false;
        }
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
            (startTime, flowRate,,) = _cfa.getFlow(_acceptedToken, address(this), _receiver);
            receiver = _receiver;
        }
    }

    event ReceiverChanged(address receiver); //what is this?

    /// @dev If a new stream is opened, or an existing one is opened
    function _updateOutflow(bytes calldata ctx)
        private
        returns (bytes memory newCtx)
    {
      newCtx = ctx;
      // @dev This will give me the new flowRate, as it is called in after callbacks
      int96 netFlowRate = _cfa.getNetFlow(_acceptedToken, address(this));
      (,int96 outFlowRate,,) = _cfa.getFlow(_acceptedToken, address(this), _receiver); // CHECK: unclear what happens if flow doesn't exist.
      int96 inFlowRate = netFlowRate + outFlowRate;

      // @dev If inFlowRate === 0, then delete existing flow.
      if (inFlowRate == int96(0)) {
        // @dev if inFlowRate is zero, delete outflow.
        newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), _receiver, _acceptedToken);
        } else if (outFlowRate != int96(0)){
        newCtx = cfaV1.updateFlowWithCtx(ctx, _receiver, _acceptedToken, inFlowRate);
      } else {
      // @dev If there is no existing outflow, then create new flow to equal inflow
        newCtx = cfaV1.createFlowWithCtx(ctx, _receiver, _acceptedToken, inFlowRate);
      }
    }

    // @dev Change the Receiver of the total flow
    function _changeReceiver( address newReceiver ) internal {
        require(newReceiver != address(0));
        // @dev because our app is registered as final, we can't take downstream apps
        require(!_host.isApp(ISuperApp(newReceiver)));
        if (newReceiver == _receiver) return ;
        // @dev delete flow to old receiver
        (,int96 outFlowRate,,) = _cfa.getFlow(_acceptedToken, address(this), _receiver); //CHECK: unclear what happens if flow doesn't exist.
        if(outFlowRate > 0){
            cfaV1.deleteFlow(address(this), _receiver, _acceptedToken);
          // @dev create flow to new receiver
            cfaV1.createFlow(_receiver, _acceptedToken, _cfa.getNetFlow(_acceptedToken, address(this)));
        }
        // @dev set global receiver to new receiver
        _receiver = newReceiver;

        emit ReceiverChanged(_receiver);
    }

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
        ISuperfluid.Context memory decompiledContext = _host.decodeCtx(_ctx);
        address senderAddress = decompiledContext.msgSender;

        (, int96 initialFlowRate,,) = _cfa.getFlow(_acceptedToken, senderAddress, address(this));

        if (initialFlowRate >= _requiredFlowRate && optionReady == true) {
            _activateOption();
            // _underlyingAsset.transfer(address(this), _underlyingAmount);
            // optionActive = true;
        }
        return _updateOutflow(_ctx);
    }

    function afterAgreementUpdated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 ,//_agreementId,
        bytes calldata, // agreementData,
        bytes calldata ,//_cbdata,
        bytes calldata _ctx
    )
        external override
        onlyExpected(_superToken, _agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        
        //check if option is currently active
        //if it's not currently active and updatedFlowRate >= _requiredFlowRate, then activate the option 
        //get flowRate from buyer
        (, int96 updatedFlowRate,,) = _cfa.getFlow(_acceptedToken, msg.sender, address(this));
        //if the new flowRate meets requirements 
        //and if we are not yet at expiration date 
        //and option is ready, but not yet active 
        //activate the option 
        if (updatedFlowRate >= _requiredFlowRate && block.timestamp < _expirationDate && optionReady == true && optionActive == false) {
            _activateOption();
        }
        
        //if option is active and flowRate from buyer is lower than the required flowRate, deactivate the option 
        else if (optionActive == true && updatedFlowRate < _requiredFlowRate) {
            _deactivateOption();
        }
        
        return _updateOutflow(_ctx);
    }

    //note - option is ineffective after termination clause is run
    function afterAgreementTerminated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32 ,//_agreementId,
        bytes calldata /*_agreementData*/,
        bytes calldata ,//_cbdata,
        bytes calldata _ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        // According to the app basic law, we should never revert in a termination callback
        if (!_isSameToken(_superToken) || !_isCFAv1(_agreementClass)) return _ctx;
        
        //deactivate the option if the flow is deleted - but don't use the function
        //change expiration date to now so that option can no longer be exercised 

         _expirationDate = block.timestamp;
        
        //set option to inactive & not ready
        optionActive = false;
        optionReady = false; 
        //use try catch - don't run transfer by itself in termination callback 
        //approve in the catch{}
        try _underlyingAsset.transfer(_receiver, _underlyingAmount)
        // solhint-disable-next-line no-empty-blocks

        {} catch {
                _underlyingAsset.approve(_receiver, _underlyingAmount);
        
        }

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
        require(msg.sender == address(_host), "RedirectAll: support only one host");
        _;
    }

    modifier onlyExpected(ISuperToken superToken, address agreementClass) {
        require(_isSameToken(superToken), "RedirectAll: not accepted token");
        require(_isCFAv1(agreementClass), "RedirectAll: only CFAv1 supported");
        _;
    }

}