pragma solidity ^0.8.0;

import {ISuperfluid, ISuperToken, ISuperApp, ISuperAgreement, SuperAppDefinitions} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol"; 

import {CFAv1Library} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

import {IConstantFlowAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {SuperAppBase} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";

import {Ownable} from "@openzeppelin/contracts/access/Ownable.sol";

import { UniV3ObservationMock } from "./test/UniV3ObservationMock.sol";

import "hardhat/console.sol";

contract EmploymentLoan is SuperAppBase, Ownable {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;

    UniV3ObservationMock public uniV3TWAP;

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

    bool public loanInitiated = false;
    bool public loanCompleted = false;
    bool public loanSolvent = true;

    ISuperfluid public host;
    IConstantFlowAgreementV1 public cfa;

    constructor(
        int _borrowAmount,
        int8 _interestRate, //annual interest rate
        int _paybackMonths,
        int _collateralAmount,
        address _employer,
        address _borrower,
        ISuperToken _borrowToken,
        ISuperToken _collateralToken,
        ISuperfluid _host,
        UniV3ObservationMock _uniV3TWAP
    ) {
        host = _host;
        cfa = IConstantFlowAgreementV1(
            address(
                host.getAgreementClass(
                    keccak256(
                        "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                    )
                )
            )
        );

        borrowAmount = _borrowAmount;
        interestRate = _interestRate;
        paybackMonths = _paybackMonths;
        collateralAmount = _collateralAmount;
        employer = _employer;
        borrower = _borrower;
        borrowToken = _borrowToken;
        collateralToken = _collateralToken;

        cfaV1 = CFAv1Library.InitData(host, cfa);
        uniV3TWAP = _uniV3TWAP;

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
        SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
        SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }


    function getPaymentFlowRate() public view returns (int96 paymentFlowRate) {
        return (int96(((borrowAmount / paybackMonths) + ((borrowAmount * int(interestRate / 100)) / paybackMonths)) / ((365 / 12) * 86400)));
    }

    function _getCollateralFlowRate() internal view returns (int96 collateralFlowRate) {
        int96 collateralDenominatedBorrowAmount;
        //note: assumes that both collateral token and borrow token HAVE underlying addresse
        int96 collateralTokenPrice = uniV3TWAP.getCurrentPrice(collateralToken.getUnderlyingToken());
        int96 borrowTokenPrice = uniV3TWAP.getCurrentPrice(borrowToken.getUnderlyingToken());
        //denominate borrow amount in collateral token instead
        if (collateralTokenPrice > 0 ) {
            collateralDenominatedBorrowAmount = int96((borrowTokenPrice / collateralTokenPrice) * borrowAmount);
        }
        //calculate monthly payment formula
        //createFlow to lender
        return (collateralDenominatedBorrowAmount / int96(paybackMonths)) +(collateralDenominatedBorrowAmount * (interestRate / 100) / int96(paybackMonths)) / ((365 / 12) * 86400);
    }

    function getTotalAmountRemaining() public view returns (uint) {
            uint timePassed = block.timestamp - uint(loanStartTime);
            if ((paybackMonths * (365 * 12) * 86400) - int(timePassed) <= 0) {
                return 0;
            }
            int remainingFlowRate = int((borrowAmount / paybackMonths) + ((borrowAmount * int(interestRate / 100) / paybackMonths))) / ((365 / 12) * 86400);

            return uint((paybackMonths * (365 * 12) * 86400) - int(timePassed) * remainingFlowRate);
    }


    function checkEmployerFlowRate() public view returns (bool) {
        (, int96 employerFlowRate, , ) = cfa.getFlow(
            borrowToken,
            employer,
            address(this)
        );

        if (employerFlowRate >= getPaymentFlowRate()) {
            return true;
        }
        else {
            return false;
        }
    }


    function sendCollateral() external {
        require(msg.sender == borrower, "only borrower sends collateral");

        if (collateralAmount > 0) {
            collateralToken.transferFrom(msg.sender, address(this), uint256(collateralAmount));
        }

        loanInitiated = true;
    }


    function lend() external {
        
        require(loanInitiated == true);
        require(checkEmployerFlowRate() == true, "employer must be streaming");
        
        if (collateralAmount > 0) {
            require(collateralToken.balanceOf(address(this)) == uint256(collateralAmount));
        }

        //needs to approve contract before running next line
        require(borrowToken.balanceOf(msg.sender) >= uint(borrowAmount), "insufficient lender bal");

        borrowToken.transferFrom(msg.sender, address(this), uint256(borrowAmount));
        //want to make sure that tokens are sent successfully first before setting lender to msg.sender
        int96 netFlowRate = cfa.getNetFlow(borrowToken, address(this));
        (, int96 outFlowRate, , ) = cfa.getFlow(borrowToken, address(this), borrower);
        int96 adjustedOutflow = (netFlowRate - outFlowRate) * -1;


        console.log("intended flow rate is %s", uint(uint96(adjustedOutflow - getPaymentFlowRate())));
        //update flow to borrower
        cfaV1.updateFlow(borrower, borrowToken, (adjustedOutflow - getPaymentFlowRate()));
        //create flow to lender
        cfaV1.createFlow(msg.sender, borrowToken, getPaymentFlowRate());

        lender = msg.sender;
        loanStartTime = block.timestamp;
    }  


     /// @dev If a new stream is opened, or an existing one is opened
     //1) get expected payment flowRte, current netflowRate, etc.
     //2) check how much the employer is sending - if they're not sending enough, revert
     //3) 
    function _updateOutflow(bytes calldata ctx)
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        int96 paymentFlowRate = getPaymentFlowRate();
        // @dev This will give me the new flowRate, as it is called in after callbacks
        int96 netFlowRate = cfa.getNetFlow(borrowToken, address(this));
        //get flow rate between this contract and the employer address

        require(checkEmployerFlowRate() == true, "employer not sending enough");
        
        //for outflow rate we need to include lender and borrower calc
        (, int96 outFlowRateLender, , ) = cfa.getFlow(borrowToken, address(this), lender);
        (, int96 outFlowRateBorrower, , ) = cfa.getFlow(borrowToken, address(this), borrower);
        int96 outFlowRate = outFlowRateLender + outFlowRateBorrower;

        int96 inFlowRate = netFlowRate + outFlowRate;

        if (inFlowRate < 0) {
            inFlowRate = inFlowRate * -1; // Fixes issue when inFlowRate is negative
        }
        // @dev If inFlow === 0 && outflowRate > 0, then delete existing flow.
        if (inFlowRate == int96(0)) {
            // console.log("deletion case is running");
            newCtx = cfaV1.deleteFlowWithCtx(newCtx, address(this), lender, borrowToken);
            //need to set loan to insolvent
            loanSolvent = false;
            //check if collateral > 0. if so 
            if (collateralAmount > 0) {
                newCtx = cfaV1.createFlowWithCtx(newCtx, lender, collateralToken, _getCollateralFlowRate());
            }
        }
        //if flow exists, update the flow
        else if (outFlowRate != int96(0)) {
            // console.log("update case is running");
            //if new inflow rate flow rate (i.e. new inflow to contract) is > 10 super tokens/month, pass user data
            if ((inFlowRate - paymentFlowRate) > 0) {
                newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
            } 
            else {
                //if inFlowRate is less than the required amount to pay interest, set to insolvent
                loanSolvent = false;
                if (collateralAmount > 0) {
                    newCtx = cfaV1.createFlowWithCtx(newCtx, lender, collateralToken, _getCollateralFlowRate());
                }
        
                newCtx = cfaV1.updateFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
            }
        } else {
            
            // @dev If there is no existing outflow, then create new flow to equal inflow
                if(inFlowRate - paymentFlowRate > 0) {
                //create flow to employee
                    newCtx = cfaV1.createFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate);
                }
                else if (inFlowRate > 0 && (inFlowRate - paymentFlowRate) < 0) {
                    //start sending flow to borrower, but don't begin loan
                    newCtx = cfaV1.createFlowWithCtx(newCtx, borrower, borrowToken, inFlowRate - paymentFlowRate);
                }
            }
        }

        //function to close a loan that is already completed
        function closeCompletedLoan() external {
            require(msg.sender == lender || getTotalAmountRemaining() == 0);

            (,int96 currentLenderFlowRate,,) = cfa.getFlow(borrowToken, address(this), lender);
            cfaV1.deleteFlow(address(this), lender, borrowToken);

            (,int96 currentFlowRate,,) = cfa.getFlow(borrowToken, address(this), borrower);
            cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
            loanCompleted = true;
        
        }

        //allows lender or borrower to close a loan
        //if the loan is paid off, or if the loan is closed by the lender, pass 0
        //if the loan is not yet paid off, pass in the required amount to close loan
        function closeOpenLoan(uint amountForPayoff) external {
            (,int96 currentLenderFlowRate,,) = cfa.getFlow(borrowToken, address(this), lender);
            (,int96 currentFlowRate,,) = cfa.getFlow(borrowToken, address(this), borrower);

            if (msg.sender == lender) {

                cfaV1.deleteFlow(address(this), lender, borrowToken);
                cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
                loanCompleted = true;
            }
            else {
    
                if (getTotalAmountRemaining() > 0) {
                    require (amountForPayoff >= (getTotalAmountRemaining()), "insufficient amount");
                    borrowToken.transferFrom(msg.sender, lender, amountForPayoff);

                    cfaV1.deleteFlow(address(this), lender, borrowToken);

                    cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
                    loanCompleted = true;
                }
                else {

                    cfaV1.deleteFlow(address(this), lender, borrowToken);
                    cfaV1.updateFlow(borrower, borrowToken, currentFlowRate + currentLenderFlowRate);
                    loanCompleted = true;
                }
            }
        }


    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) 
    {
        newCtx = _updateOutflow(ctx);
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) 
    {
        newCtx = _updateOutflow(ctx);
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    ) external override returns (bytes memory newCtx) 
    {
        newCtx = _updateOutflow(ctx);
    }
}