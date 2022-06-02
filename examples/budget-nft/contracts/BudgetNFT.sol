// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import {ERC721} from "@openzeppelin/contracts/token/ERC721/ERC721.sol";

import {Ownable} from "@openzeppelin/contracts/access/Ownable.sol";

import {ISuperfluid, ISuperToken, ISuperApp} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import {IConstantFlowAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import {CFAv1Library} from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";

// Simple contract which allows users to create NFTs with attached streams

contract BudgetNFT is ERC721, Ownable {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1; //initialize cfaV1 variable

    ISuperToken public _acceptedToken; // accepted token

    mapping(uint256 => int96) public flowRates;

    uint256 public nextId; // this is so we can increment the number (each stream has new id we store in flowRates)

    constructor(
        string memory _name,
        string memory _symbol,
        ISuperfluid host,
        ISuperToken acceptedToken
    ) ERC721(_name, _symbol) {
        _acceptedToken = acceptedToken;

        nextId = 0;

        assert(address(host) != address(0));
        assert(address(cfaV1.cfa) != address(0));
        assert(address(_acceptedToken) != address(0));

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

    event NFTIssued(uint256 tokenId, address receiver, int96 flowRate);

    // @dev creates the NFT, but it remains in the contract

    function issueNFT(address receiver, int96 flowRate) external onlyOwner {
        _issueNFT(receiver, flowRate);
    }

    function _issueNFT(address receiver, int96 flowRate) internal {
        require(receiver != address(this), "Issue to a new address");
        require(flowRate > 0, "flowRate must be positive!");

        flowRates[nextId] = flowRate;
        emit NFTIssued(nextId, receiver, flowRates[nextId]);
        _mint(receiver, nextId);
        nextId += 1;
    }

    // @dev owner can edit the NFT as long as it hasn't been issued (transferred out) yet
    function editNFT(uint256 tokenId, int96 flowRate) external onlyOwner exists(tokenId) {
        require(flowRate >= 0, "flowRate must be positive!");

        address receiver = ownerOf(tokenId);

        if (flowRate == 0) {
            // subtract previous flowrate
            _reduceFlow(receiver, flowRates[tokenId]);
        } else {
            // add new flowRate
            _increaseFlow(receiver, flowRate - flowRates[tokenId]);
        }

        flowRates[tokenId] = flowRate;
    }

    function burnNFT(uint256 tokenId) external onlyOwner exists(tokenId) {
        address receiver = ownerOf(tokenId);

        int96 rate = flowRates[tokenId];
        delete flowRates[tokenId];
        _burn(tokenId);
        //deletes flow to previous holder of nft & receiver of stream after it is burned

        //we will reduce flow of owner of NFT by total flow rate that was being sent to owner of this token
        _reduceFlow(receiver, rate);
    }

    //this hook will execute every time the token is transferred
    //When the token is first "issued", i.e. moved from the first contract, it will start the stream
    function _beforeTokenTransfer(
        address oldReceiver,
        address newReceiver,
        uint256 tokenId
    ) internal override {
        //blocks transfers to superApps - done for simplicity, but you could support super apps in a new version!
        require(
            !cfaV1.host.isApp(ISuperApp(newReceiver)) ||
                newReceiver == address(this),
            "New receiver can not be a superApp"
        );

        // @dev delete flowRate of this token from old receiver
        // ignores minting case
        _reduceFlow(oldReceiver, flowRates[tokenId]);
        // @dev create flowRate of this token to new receiver
        // ignores return-to-issuer case
        _increaseFlow(newReceiver, flowRates[tokenId]);
    }

    // Add a function that allows a token owner to split their token into two streams
    function splitStream(uint256 tokenId, int96 newTokenFlowRate) public {
        require(
            msg.sender == ownerOf(tokenId),
            "can't edit someone else's stream"
        );
        require(
            newTokenFlowRate < flowRates[tokenId],
            "new flow must be less than old flow"
        );

        //reduce the flow to the receiver by the 'flowRate' in storage
        flowRates[tokenId] -= newTokenFlowRate;
        _reduceFlow(msg.sender, newTokenFlowRate);
        // mint new token - will create new token's flow rate
        _issueNFT(msg.sender, newTokenFlowRate);
        // change old token's stored flowRate
        //decrease by the value of newToken flow rate (which must be less than the old flow so can't be negative)

        // create new token's stored flowRate
    }

    function mergeStreams(uint256 tokenId1, uint256 tokenId2) public {
        require(
            msg.sender == ownerOf(tokenId1),
            "Can't edit someone else's stream"
        );
        require(
            msg.sender == ownerOf(tokenId2),
            "Can't edit someone else's stream"
        );

        //merge token1 into token2
        //increase flowRate of token1
        flowRates[tokenId1] += flowRates[tokenId2];
        //delete flowRate of token 2 and burn NFT
        delete flowRates[tokenId2];
        _burn(tokenId2);
    }

    /**************************************************************************
     * Modifiers
     *************************************************************************/

    modifier exists(uint256 tokenId) {
        require(_exists(tokenId), "token doesn't exist or has been burnt");
        _;
    }

    /**************************************************************************
     * Library
     *************************************************************************/
    //this will reduce the flow or delete it
    function _reduceFlow(address to, int96 flowRate) internal {
        if (to == address(this)) return;

        (, int96 outFlowRate, , ) = cfaV1.cfa.getFlow(
            _acceptedToken,
            address(this),
            to
        );

        if (outFlowRate == flowRate) {
            cfaV1.deleteFlow(address(this), to, _acceptedToken);
        } else if (outFlowRate > flowRate) {
            // reduce the outflow by flowRate;
            // shouldn't overflow, because we just checked that it was bigger.
            cfaV1.updateFlow(to, _acceptedToken, outFlowRate - flowRate);
        }
        // won't do anything if outFlowRate < flowRate
    }

    //this will increase the flow or create it
    function _increaseFlow(address to, int96 flowRate) internal {
        if (to == address(0)) return;

        (, int96 outFlowRate, , ) = cfaV1.cfa.getFlow(
            _acceptedToken,
            address(this),
            to
        ); //returns 0 if stream doesn't exist
        if (outFlowRate == 0) {
            cfaV1.createFlow(to, _acceptedToken, flowRate);
        } else {
            // increase the outflow by flowRates[tokenId]
            cfaV1.updateFlow(to, _acceptedToken, outFlowRate + flowRate);
        }
    }

}