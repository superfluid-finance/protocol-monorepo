// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBase.sol";
import {
    IInstantDistributionAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";

import {IDAv1Library} from "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract TokenSpreader {

    ISuperToken public spreaderToken;                  // Token to be distributed to unit holders by distribute() function

    using IDAv1Library for IDAv1Library.InitData;      // Creating idaV1 object for easy use of IDA functions
    IDAv1Library.InitData public idaV1;

    uint32 public constant INDEX_ID = 0;               // The IDA Index. Since this contract will only use one index, we'll hardcode it to "0".

    constructor(
        ISuperfluid _host,
        ISuperToken _spreaderToken
    ) {

        // Ensure _spreaderToken is indeed a super token
        require(address(_host) == _spreaderToken.getHost(),"!superToken"); 


        spreaderToken = _spreaderToken;

        // Initializing the host and agreement type in the idaV1 object so the object can have them on hand for enacting IDA functions
        // Read more on initialization: https://docs.superfluid.finance/superfluid/developers/solidity-examples/solidity-libraries/idav1-library#importing-and-initialization
        idaV1 = IDAv1Library.InitData(
            _host,
            IInstantDistributionAgreementV1(
                address(_host.getAgreementClass(keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1")))
            )
        );

        // Creates the IDA Index through which tokens will be distributed
        idaV1.createIndex(_spreaderToken, INDEX_ID);

    }

    /**************************************************************************
     * IDA Operations
     *************************************************************************/

    /// @notice Takes the entire balance of the designated spreaderToken in the contract and distributes it out to unit holders w/ IDA
    function distribute() public {

        uint256 spreaderTokenBalance = spreaderToken.balanceOf(address(this));

        (uint256 actualDistributionAmount,) = idaV1.ida.calculateDistribution(
            spreaderToken,
            address(this), 
            INDEX_ID,
            spreaderTokenBalance
        );

        idaV1.distribute(spreaderToken, INDEX_ID, actualDistributionAmount);

    }

    /// @notice lets an account gain a single distribution unit
    /// @param subscriber subscriber address whose units are to be incremented
    function gainShare(address subscriber) public {

        // Get current units subscriber holds
        (,,uint256 currentUnitsHeld,) = idaV1.getSubscription(
            spreaderToken,   
            address(this),   
            INDEX_ID,        
            subscriber       
        );

        // Update to current amount + 1
        idaV1.updateSubscriptionUnits(
            spreaderToken,
            INDEX_ID,
            subscriber,
            uint128(currentUnitsHeld + 1)
        );

    }

    /// @notice lets an account lose a single distribution unit
    /// @param subscriber subscriber address whose units are to be decremented
    function loseShare(address subscriber) public {

        // Get current units subscriber holds
        (,,uint256 currentUnitsHeld,) = idaV1.getSubscription(
            spreaderToken,   
            address(this),   
            INDEX_ID,        
            subscriber       
        );

        // Update to current amount - 1 (reverts if currentUnitsHeld - 1 < 0, so basically if currentUnitsHeld = 0)
        idaV1.updateSubscriptionUnits(
            spreaderToken,
            INDEX_ID,
            subscriber,
            uint128(currentUnitsHeld - 1)
        );

    }

    /// @notice allows an account to delete its entire subscription this contract
    /// @param subscriber subscriber address whose subscription is to be deleted
    function deleteShares(address subscriber) public {

        idaV1.deleteSubscription(
            spreaderToken,
            address(this),
            INDEX_ID,
            subscriber
        );

    }

}