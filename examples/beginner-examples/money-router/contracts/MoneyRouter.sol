//SPDX-License-Identifier: Unlicense
pragma solidity 0.8.13;

import "hardhat/console.sol";

import {ERC721} from "@openzeppelin/contracts/token/ERC721/ERC721.sol";

import {Ownable} from "@openzeppelin/contracts/access/Ownable.sol";

import { ISuperfluid, ISuperToken, ISuperApp } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import { ISuperfluidToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidToken.sol";

import {IConstantFlowAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {CFAv1Library} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

contract MoneyRouter {
    address public owner;

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1; //initialize cfaV1 variable
    
    mapping (address => bool) public accountList;

    constructor(ISuperfluid host, address _owner) {

        assert(address(host) != address(0));
        console.log("Deploying a Money Router with owner:", owner);
        owner = _owner;

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

    function whitelistAccount(address _account) external {
        require(msg.sender == owner, "only owner can whitelist accounts");
        accountList[_account] = true;
    }

    function removeAccount(address _account) external {
        require(msg.sender == owner, "only owner can remove accounts");
        accountList[_account] = false;
    }

    function changeOwner(address _newOwner) external {
        require(msg.sender == owner, "only owner can change ownership");
        owner = _newOwner;
    }

    function sendLumpSumToContract(ISuperToken token, uint amount) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");
        token.transferFrom(msg.sender, address(this), amount);
    }

    function createFlowIntoContract(ISuperfluidToken token, int96 flowRate) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");

        cfaV1.createFlowByOperator(msg.sender, address(this), token, flowRate);
    }

    function updateFlowIntoContract(ISuperfluidToken token, int96 newFlowRate) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");

        cfaV1.updateFlowByOperator(msg.sender, address(this), token, newFlowRate);
    }

    function deleteFlowIntoContract(ISuperfluidToken token) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");

        cfaV1.deleteFlow(msg.sender, address(this), token);
    }

    function withdrawFunds(ISuperToken token, uint amount) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");
        token.transfer(msg.sender, amount);
    }

    function createFlowFromContract(ISuperfluidToken token, address receiver, int96 flowRate) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");
        cfaV1.createFlow(receiver, token, flowRate);
    }

    function updateFlowFromContract(ISuperfluidToken token, address receiver, int96 newFlowRate) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");
        cfaV1.updateFlow(receiver, token, newFlowRate);
    }

    function deleteFlowFromContract(ISuperfluidToken token, address receiver) external {
        require(msg.sender == owner || accountList[msg.sender] == true, "must be authorized");
        cfaV1.deleteFlow(address(this), receiver, token);
    }
}
