pragma solidity 0.6.6;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/**
 * @title Superfluid's token interface
 * @author Superfluid
 */
abstract contract ISuperToken is IERC20 {

    /*
    *   Agreement Account States
    */

    /// @notice Get state of Agreement Account
    /// @param account Account to query
    function getAgreementAccountState(
        address account
    )
        external
        virtual
        view
        returns (bytes memory data);

    /// @notice Update Account state
    /// @param account Account of the agrement
    /// @param state Agreement state of the account
    function updateAgreementAccountState(
        address account,
        bytes calldata state
    )
        external
        virtual;

    /*
    * Agreement Data
    */

    /// @notice Register or update a agreement TODO fix comments
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function createAgreement(
        address agreementClass,
        bytes32 id,
        bytes calldata data
    )
        external
        virtual;

    /// @notice Get data from agreement
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        virtual
        view
        returns(bytes memory state);

    /// @notice Close Agreement
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function terminateAgreement(
        address agreementClass,
        bytes32 id
    )
        external
        virtual;


    /*
    * SuperToken
    */

    /// @notice Upgrade ERC20 to SuperToken.
    /// @dev Will use ´transferFrom´ to get tokens. Before calling this
    ///      function you should ´approve´ this contract
    /// @param amount Number of tokens to be upgraded
    function upgrade(uint256 amount) external virtual;


    /// @notice Downgrade SuperToken to ERC20.
    /// @dev Will call transfer to send tokens
    /// @param amount Number of tokens to be downgraded
    function downgrade(uint256 amount) external virtual;

}
