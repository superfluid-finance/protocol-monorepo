// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.0;

import {ISuperfluid, ISuperToken, ISuperApp, ISuperAgreement, SuperAppDefinitions} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol"; 

import { AggregatorV3Interface } from "@chainlink/contracts/src/v0.8/interfaces/AggregatorV3Interface.sol";

import { EmploymentLoan } from "./EmploymentLoan.sol";


contract LoanFactory {

    uint loanId = 0;
    mapping (uint => EmploymentLoan) public idToLoan;
    mapping (address => uint) public employmentLoanOwners;

    function createNewLoan(
        int _borrowAmount,
        int8 _interestRate,
        int8 _paybackMonths,
        int _collateralAmount,
        address _employer,
        address _borrower,
        ISuperToken _borrowToken,
        ISuperToken _collateralToken,
        ISuperfluid _host,
        AggregatorV3Interface _priceFeed,
        uint8 _priceFeedDecimals

    ) external returns (uint) {
        
        EmploymentLoan newLoan = new EmploymentLoan(
            _borrowAmount,
            _interestRate,
            _paybackMonths,
            _collateralAmount,
            _employer,
            _borrower,
            _borrowToken,
            _collateralToken,
            _host,
            _priceFeed,
            _priceFeedDecimals
        );

        loanId++;
    
        idToLoan[loanId] = newLoan;
        employmentLoanOwners[msg.sender] = loanId;

        return loanId;
        
    }

    function getLoanAddressByID(uint _id) public view returns (EmploymentLoan) {
        return idToLoan[_id];
    }

    function getLoanByOwner(address _owner) public view returns (uint) {
        return employmentLoanOwners[_owner];
    }

}