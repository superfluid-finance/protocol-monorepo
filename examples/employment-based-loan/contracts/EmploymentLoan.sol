// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.0;

import { ISuperfluid, ISuperToken, ISuperApp, ISuperAgreement, SuperAppDefinitions } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import { SuperAppBase } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";

import { AggregatorV3Interface } from "@chainlink/contracts/src/v0.8/interfaces/AggregatorV3Interface.sol";

contract EmploymentLoan is SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;

    bytes32 constant CFA_ID = keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");

    AggregatorV3Interface public priceFeed;
    //decimals of value returned by the priceFeed
    uint public priceFeedDecimals;

    uint public loanStartTime;
    int public borrowAmount;
    int8 public interestRate;
    int public paybackMonths;
    int public collateralAmount;
    address public employer;
    address public borrower;
    address public lender;

    ISuperToken public borrowToken;
    ISuperToken public collateralToken;

    constructor(
        int _borrowAmount,
        int8 _interestRate, //annual interest rate, in whole number - i.e. 8% would be passed as 8
        int _paybackMonths,
        int _collateralAmount,
        address _employer,
        address _borrower,
        ISuperToken _borrowToken,
        ISuperToken _collateralToken,
        ISuperfluid _host,
        AggregatorV3Interface _priceFeed,
        uint _priceFeedDecimals
    ) {
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(
            address(_host.getAgreementClass(CFA_ID))
        );

        borrowAmount = _borrowAmount;
        interestRate = _interestRate;
        paybackMonths = _paybackMonths;
        collateralAmount = _collateralAmount;
        employer = _employer;
        borrower = _borrower;
        borrowToken = _borrowToken;
        collateralToken = _collateralToken;

        cfaV1 = CFAv1Library.InitData(_host, cfa);
        priceFeed = _priceFeed;
        priceFeedDecimals = _priceFeedDecimals;

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
        SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function getPaymentFlowRate() public view returns (int96 paymentFlowRate) {
        return (int96(((borrowAmount / paybackMonths) + ((borrowAmount * int(int(interestRate) / 100)) / paybackMonths)) / ((365 / 12) * 86400)));
    }

    function _getCollateralFlowRate() public view returns (int96 collateralFlowRate) {
        int collateralDenominatedBorrowAmount;
        (, int collateralTokenPrice,,,) = priceFeed.latestRoundData();

        //note: all chainlink feeds return either 8 or 18 decimals...in our case, if it's not 18, we need to balance out the diff
        if (uint(priceFeedDecimals) < 18) {
            collateralTokenPrice = int(uint(collateralTokenPrice) * (10 ** uint(18 - int(priceFeedDecimals))));
        }

        //denominate borrow amount in collateral token instead
        if (collateralTokenPrice > 0 ) {
            //not perfect, but assumes that borrow token is a stablecoin
            collateralDenominatedBorrowAmount = int((borrowAmount * collateralTokenPrice) / borrowAmount);
        }

        //calculate monthly payment formula
        return (int96(((collateralDenominatedBorrowAmount / paybackMonths) + ((collateralDenominatedBorrowAmount * int(int(interestRate) / 100)) / paybackMonths)) / ((365 / 12) * 86400)));
    }

    function getTotalAmountRemaining() public view returns (uint) {
        //if there is no time left on loan, return zero
        int secondsLeft = (paybackMonths * int((365 * 86400) / 12)) - int(block.timestamp - loanStartTime);
        if (secondsLeft <= 0) {
            return 0;
        }
        //if an amount is left, return the total amount to be paid
        else {
            return uint(secondsLeft) * uint(int(getPaymentFlowRate()));
        }
    }

    //allows loan to be prepped by the borrower
    function sendCollateral() external {
        require(msg.sender == borrower);

        if (collateralAmount > 0) {
            collateralToken.transferFrom(msg.sender, address(this), uint256(collateralAmount));
        }
    }

    //lender can use this function to send funds to the borrower and start the loan
    function lend() external {

        (, int96 employerFlowRate, , ) = cfaV1.cfa.getFlow(
            borrowToken,
            employer,
            address(this)
        );

        require(employerFlowRate >= getPaymentFlowRate());

        if (collateralAmount > 0) {
            require(collateralToken.balanceOf(address(this)) >= uint256(collateralAmount));
        }
        //lender must approve contract before running next line
        borrowToken.transferFrom(msg.sender, borrower, uint256(borrowAmount));
        //want to make sure that tokens are sent successfully first before setting lender to msg.sender
        int96 netFlowRate = cfaV1.cfa.getNetFlow(borrowToken, address(this));
        (, int96 outFlowRate, , ) = cfaV1.cfa.getFlow(borrowToken, address(this), borrower);

        //update flow to borrower
        cfaV1.updateFlow(borrower, borrowToken, ((netFlowRate - outFlowRate) * -1) - getPaymentFlowRate());
        //create flow to lender
        cfaV1.createFlow(msg.sender, borrowToken, getPaymentFlowRate());

        lender = msg.sender;
        loanStartTime = block.timestamp;
    }

     ///If a new stream is opened, or an existing one is opened
     //1) get expected payment flowRte, current netflowRate, etc.
     //2) check how much the employer is sending - if they're not sending enough, revert

     function _updateOutFlowCreate(
         bytes calldata ctx,
         int96 paymentFlowRate,
         int96 collateralFlow,
         int96 inFlowRate
         )
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        // @dev If there is no existing outflow, then create new flow to equal inflow
        if(inFlowRate - paymentFlowRate > 0 && collateralFlow == 0) {
        //create flow to employee
            newCtx = cfaV1.createFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
        }
        //collateral net flow is < 0 - meaning that it exists, and the new payment flow rate is > 0
        else if (collateralFlow > 0 && inFlowRate - paymentFlowRate > 0) {
            newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), lender, collateralToken);
            newCtx = cfaV1.createFlowWithCtx(newCtx, lender, borrowToken, paymentFlowRate);
            newCtx = cfaV1.createFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
        }
    }

    function _updateOutFlowUpdate(
        bytes calldata ctx,
        int96 paymentFlowRate,
        int96 outFlowRateLender,
        int96 collateralFlow,
        int96 inFlowRate
        )
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        //this will get us the amount of money that should be redirected to the lender out of the inflow, denominated in borrow token
        //if the amount being sent is enough to cover loan
        if ((inFlowRate - paymentFlowRate) > 0) {
            //if there is a collateral net flow
            if(collateralFlow > 0) {
                //loan is solvent again so delete the flow of collateral to lender
                newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), lender, collateralToken);
                //re open payment flow to lender
                newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
                newCtx = cfaV1.createFlowWithCtx(newCtx, lender, borrowToken, paymentFlowRate);
            }
            //if the loan is solvent flow when the flow is updated, and it is already started, then simply update outflow toborrower
            else if (collateralFlow == 0 && outFlowRateLender > 0) {
                newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
            }
            else {
                //otherwise, if there is no colleteral netflow and loan has not yet begun, then just
                //update the flow to the borrower with the full inflow rate
                newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
            }
        }

        else {
            //if inFlowRate is less than the required amount to pay interest, we need to start streaming out the collateral
            if (collateralAmount > 0) {
                if(outFlowRateLender == 0 && collateralFlow == 0) {
                    //if current outflow rate to lender is zero, just update the flow to borrower
                    newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
                }
                else {
                    //the borrow token amount has been reduced below our threshold, so we must:
                    //update flow to borrower to reflect inflow amount
                    //begin streaming out the collateral
                    newCtx = cfaV1.createFlowWithCtx(newCtx, lender, collateralToken, _getCollateralFlowRate());
                    newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), lender, borrowToken);
                    newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
                }
            }
            //if there is no collateral amount, then just update the flow
            else {
                if (outFlowRateLender > 0) {
                    newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
                }
                else {
                    //if outflow to lender does not yet exist, then update accordingly
                    newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
                }
            }
        }
    }

    function _updateOutFlowDelete(
        bytes calldata ctx,
        int96 outFlowRateLender
    )
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;

        //delete flow to lender in borrow token if they are currently receiving a flow
        if(outFlowRateLender > 0) {
            newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), lender, borrowToken);
        }
        //delete flow to borrower in borrow token
        newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), borrower, borrowToken);
        //check if collateral > 0. if so, create a flow to lender using the collateral
        if (collateralAmount > 0) {
            newCtx = cfaV1.createFlowWithCtx(newCtx, lender, collateralToken, _getCollateralFlowRate());
        }
    }

    function _updateOutflow(bytes calldata ctx)
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        //this will get us the amount of money that should be redirected to the lender out of the inflow, denominated in borrow token
        int96 paymentFlowRate = getPaymentFlowRate();
        // @dev This will give me the new flowRate, as it is called in after callbacks
        int96 netFlowRate = cfaV1.cfa.getNetFlow(borrowToken, address(this));

        //current amount being sent to lender
        (, int96 outFlowRateLender, , ) = cfaV1.cfa.getFlow(borrowToken, address(this), lender);
        //current amount being sent to borrower
        (, int96 outFlowRateBorrower, , ) = cfaV1.cfa.getFlow(borrowToken, address(this), borrower);
        //current amount being streamed out in collateral token
        (, int96 collateralFlow, , ) = cfaV1.cfa.getFlow(collateralToken, address(this), lender);
        //total outflow rate in borrow token - only 2
        int96 outFlowRate = outFlowRateLender + outFlowRateBorrower;
        //total inflow rate in borrow token
        int96 inFlowRate = netFlowRate + outFlowRate;

        if (inFlowRate < 0) {
            inFlowRate = inFlowRate * -1; // Fixes issue when inFlowRate is negative
        }

        // @dev If inFlow === 0 && outflowRate > 0, then delete existing flows.
        if (inFlowRate == int96(0)) {
            newCtx = _updateOutFlowDelete(ctx, outFlowRateLender);
        }
        //if flow exists, update the flow according to various params
        else if (outFlowRate != int96(0)) {
            newCtx = _updateOutFlowUpdate(ctx, paymentFlowRate, outFlowRateLender, collateralFlow, inFlowRate);
        }
        //no flow exists into the contract in borrow token
        else {
            newCtx = _updateOutFlowCreate(ctx, paymentFlowRate, collateralFlow, inFlowRate);
            // @dev If there is no existing outflow, then create new flow to equal inflow
            }
    }

    //function to close a loan that is already completed
    function closeCompletedLoan() external {
        require(msg.sender == lender || getTotalAmountRemaining() <= 0);

        uint collateralTokenBalance = collateralToken.balanceOf(address(this));
        if (collateralAmount >= 0 && collateralTokenBalance > 0) {
            collateralToken.transfer(borrower, collateralTokenBalance);
        }

        (,int96 currentLenderFlowRate,,) = cfaV1.cfa.getFlow(borrowToken, address(this), lender);
        cfaV1.deleteFlow(address(this), lender, borrowToken);

        (,int96 currentFlowRate,,) = cfaV1.cfa.getFlow(borrowToken, address(this), borrower);
        cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
    }
    //allows lender or borrower to close a loan
    //if the loan is paid off, or if the loan is closed by the lender, pass 0
    //if the loan is not yet paid off, pass in the required amount to close loan
    function closeOpenLoan(uint amountForPayoff) external {
        (,int96 currentLenderFlowRate,,) = cfaV1.cfa.getFlow(borrowToken, address(this), lender);
        (,int96 currentFlowRate,,) = cfaV1.cfa.getFlow(borrowToken, address(this), borrower);

        if (msg.sender == lender) {
            cfaV1.deleteFlow(address(this), lender, borrowToken);
            cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
        }
        else {

            if (getTotalAmountRemaining() > 0) {
                require (amountForPayoff >= (getTotalAmountRemaining()), "insuf funds");
                borrowToken.transferFrom(msg.sender, lender, amountForPayoff);

                cfaV1.deleteFlow(address(this), lender, borrowToken);

                cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
            }
            else {

                cfaV1.deleteFlow(address(this), lender, borrowToken);
                cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
            }
        }
    }

    function afterAgreementCreated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyCFA(_agreementClass)
        returns (bytes memory newCtx)
    {
        newCtx = _updateOutflow(ctx);
    }

    function afterAgreementUpdated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyCFA(_agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        newCtx = _updateOutflow(ctx);
    }

    function afterAgreementTerminated(
        ISuperToken _superToken,
        address _agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyHost
        returns (bytes memory newCtx)
    {
        if (!_isCFAv1(_agreementClass)) {
            return ctx;
        }
        return _updateOutflow(ctx);
    }

    function _isCFAv1(address agreementClass) private view returns (bool) {
        return ISuperAgreement(agreementClass).agreementType() == CFA_ID;
    }

    modifier onlyHost() {
        require(
            msg.sender == address(cfaV1.host),
            "Only host can call callback"
        );
        _;
    }

    modifier onlyCFA(address agreementClass) {
        require(_isCFAv1(agreementClass), "Only CFAv1 supported");
        _;
    }
}