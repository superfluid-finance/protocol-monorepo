pragma solidity >= 0.6.0;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/**
 * @title Superfluid's token interface
 * @author Superfluid
 */
abstract contract ISuperToken is IERC20 {

    /*
     * Agreement functions
     */

    /// @notice Create a new agreement
    /// @param id Agreement ID
    /// @param data Agreement data
    function createAgreement(
        bytes32 id,
        bytes calldata data
    )
        external
        virtual;

    /// @notice Agreement creation event
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    event AgreementCreated(
        address indexed agreementClass,
        bytes32 id
    );

    /// @notice Get data of the agreement
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
    /// @param id Agreement ID
    function terminateAgreement(
        bytes32 id
    )
        external
        virtual;

    /// @notice Agreement termination event
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    event AgreementTerminated(
        address indexed agreementClass,
        bytes32 id
    );

    /// @notice Update Account state
    /// @param account Account of the agrement
    /// @param state Agreement state of the account
    function updateAgreementAccountState(
        address account,
        bytes calldata state
    )
        external
        virtual;

    event AgreementAccountStateUpdated(
        address indexed agreementClass,
        address indexed account
    );

    /// @notice Get state of Agreement Account
    /// @param agreementClass Contract address of the agreement
    /// @param account Account to query
    function getAgreementAccountState(
        address agreementClass,
        address account
    )
        external
        virtual
        view
        returns (bytes memory data);


    /*
     * Account functions
     */
     /// @notice Get a list of agreements that is active for the account
     /// @dev An active agreement is one that has state for the account
     /// @param account Account to query
    function getAccountActiveAgreements(address account)
        public
        virtual
        view
        returns(address[] memory);

    /// @notice Calculate the real balance of a user, taking in consideration
    ///         all agreements of the account
    /// @dev It is used by solvency agent to predict future balance of the account
    /// @param account for the query
    /// @param timestamp Time of balance
    /// @param account Account to query
     function realtimeBalanceOf(
         address account,
         uint256 timestamp
     )
         external
         virtual
         view
         returns (int256);

    /*
     * ERC20 compatability functions
     */

    /// @notice Upgrade ERC20 to SuperToken.
    /// @dev It will use ´transferFrom´ to get tokens. Before calling this
    ///      function you should ´approve´ this contract
    /// @param amount Number of tokens to be upgraded
    function upgrade(uint256 amount) external virtual;

    /// @notice Token upgrade event
    /// @param account Account whose tokens are upgraded
    /// @param amount Amount of tokens upgraded
    event TokenUpgraded(
        address indexed account,
        uint256 amount
    );

    /// @notice Downgrade SuperToken to ERC20.
    /// @dev It will call transfer to send tokens
    /// @param amount Number of tokens to be downgraded
    function downgrade(uint256 amount) external virtual;

    /// @notice Token downgrade event
    /// @param account Account whose tokens are upgraded
    /// @param amount Amount of tokens downgraded
    event TokenDowngraded(
        address indexed account,
        uint256 amount
    );

}
