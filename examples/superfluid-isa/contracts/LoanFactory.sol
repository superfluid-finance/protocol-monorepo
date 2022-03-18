pragma solidity ^0.8.0;

import {ISuperfluid, ISuperToken, ISuperApp, ISuperAgreement, SuperAppDefinitions} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol"; 

import { UniV3ObservationMock } from "./test/UniV3ObservationMock.sol";

import { EmploymentLoan } from "./EmploymentLoan.sol";


contract LoanFactory {

    uint loanId = 0;
    mapping (uint => EmploymentLoan) public idToLoan;
    mapping (address => uint) public employmentLoanOwners;

    //maybe add a kind of protocol fee later?

    function createNewLoan(
        int _borrowAmount,
        int8 _interestRate,
        int _paybackMonths,
        int _collateralAmount,
        address _employer,
        address _borrower,
        ISuperToken _borrowToken,
        ISuperToken _collateralToken,
        ISuperfluid _host,
        UniV3ObservationMock _uniV3TWAP
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
            _uniV3TWAP
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