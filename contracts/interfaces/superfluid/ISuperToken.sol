// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { ERC20WithTokenInfo } from "../tokens/ERC20WithTokenInfo.sol";

/**
 * @title Superfluid's token interface.
 *
 * @author Superfluid
 */
abstract contract ISuperToken is ERC20WithTokenInfo {

    /**************************************************************************
     * Agreement hosting functions
     *************************************************************************/

    /**
     * @dev Create a new agreement
     * @param id Agreement ID
     * @param data Agreement data
     */
    function createAgreement(
        bytes32 id,
        bytes32[] calldata data
    )
        //onlyAgreement
        external virtual;

    /**
     * @dev Agreement creation event
     * @param agreementClass Contract address of the agreement
     * @param id Agreement ID
     * @param data Agreement data
     */
    event AgreementCreated(
        address indexed agreementClass,
        bytes32 id,
        bytes32[] data
    );

    /**
     * @dev Get data of the agreement
     * @param agreementClass Contract address of the agreement
     * @param id Agreement ID
     * @return data Data of the agreement
     */
    function getAgreementData(
        address agreementClass,
        bytes32 id,
        uint dataLength
    )
        external virtual view
        returns(bytes32[] memory data);

    /**
     * @dev Create a new agreement
     * @param id Agreement ID
     * @param data Agreement data
     */
    function updateAgreementData(
        bytes32 id,
        bytes32[] calldata data
    )
        //onlyAgreement
        external virtual;

    /**
     * @dev Agreement creation event
     * @param agreementClass Contract address of the agreement
     * @param id Agreement ID
     * @param data Agreement data
     */
    event AgreementUpdated(
        address indexed agreementClass,
        bytes32 id,
        bytes32[] data
    );

    /**
     * @dev Close the agreement
     * @param id Agreement ID
     */
    function terminateAgreement(
        bytes32 id,
        uint dataLength
    )
        //onlyAgreement
        external virtual;

    /**
     * @dev Agreement termination event
     * @param agreementClass Contract address of the agreement
     * @param id Agreement ID
     */
    event AgreementTerminated(
        address indexed agreementClass,
        bytes32 id
    );

    /**
     * @dev Liquidate the Aagreement
     * @param liquidator Address of the executer of liquidation
     * @param id Agreement ID
     * @param account Account of the agrement
     * @param deposit Deposit from the account that is going to taken as penalty
     */
    function liquidateAgreement
    (
        address liquidator,
        bytes32 id,
        address account,
        uint256 deposit
    )
        // onlyAgreement
        external virtual;

    /**
     * @dev Update agreement state slot
     * @param account Account to be updated
     *
     * NOTE
     * - To clear the storage out, provide zero-ed array of intended length
     */
    function updateAgreementStateSlot(
        address account,
        uint256 slotId,
        bytes32[] calldata slotData
    )
        //onlyAgreement
        external virtual;

    /**
     * @dev Agreement account state updated event
     * @param agreementClass Contract address of the agreement
     * @param account Account updated
     * @param slotId slot id of the agreement state
     */
    event AgreementStateUpdated(
        address indexed agreementClass,
        address indexed account,
        uint256 slotId
    );

    /**
     * @dev Get data of the slot of the state of a agreement
     * @param agreementClass Contract address of the agreement
     * @param account Account to query
     * @param slotId slot id of the state
     * @param dataLength length of the state data
     */
    function getAgreementStateSlot(
        address agreementClass,
        address account,
        uint256 slotId,
        uint dataLength
    )
        external virtual view
        returns (bytes32[] memory slotData);

    /**
     * @dev Agreement liquidation event
     * @param agreementClass Contract address of the agreement
     * @param id Agreement ID
     * @param penaltyAccount Account of the agreement
     * @param rewardAccount Account that collect the reward
     * @param deposit Amount of liquidation fee collected
     */
    event AgreementLiquidated(
        address indexed agreementClass,
        bytes32 id,
        address indexed penaltyAccount,
        address indexed rewardAccount,
        uint256 deposit
    );

    /**
     * @dev Agreement account state updated event
     * @param agreementClass Contract address of the agreement
     * @param account Account of the agrement
     * @param state Agreement state of the account
     */
    event AgreementAccountStateUpdated(
        address indexed agreementClass,
        address indexed account,
        bytes state
    );

    /**
     * @dev Settle balance from an account by the agreement.
     *      The agreement needs to make sure that the balance delta is balanced afterwards
     * @param account Account to query.
     * @param delta Amount of balance delta to be settled
     */
    function settleBalance(
        address account,
        int256 delta
    )
        // onlyAgreement
        external virtual;

    /**************************************************************************
     * Account functions
     *************************************************************************/

    /**
     * @dev Get a list of agreements that is active for the account
     * @dev An active agreement is one that has state for the account
     * @param account Account to query
     * @return activeAgreements List of accounts that have non-zero states for the account
     */
    function getAccountActiveAgreements(address account)
        external
        virtual
        view
        returns(address[] memory activeAgreements);

    /**
     * @dev Check if one account is insolvent
     * @param account Account check if is insolvent
     * @return isInsolvent Is the account insolvent?
     */
    function isAccountInsolvent(
        address account
    )
        external
        view
        virtual
        returns(bool isInsolvent);

    /**
     * @dev Calculate the real balance of a user, taking in consideration all agreements of the account
     * @param account for the query
     * @param timestamp Time of balance
     * @param account Account to query
     * @return availableBalance Real-time balance
     * @return deposit Account deposit
     * @return owedDeposit Account owed Deposit
     */
     function realtimeBalanceOf(
         address account,
         uint256 timestamp
     )
         external
         virtual
         view
         returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit);

    /**************************************************************************
     * ERC20 wrapping
     *************************************************************************/

    /**
     * @dev Return the underlaying token contract
     * @return tokenAddr Underlying token address
     */
    function getUnderlayingToken() external virtual view returns(address tokenAddr);

    /**
     * @dev Upgrade ERC20 to SuperToken.
     * @param amount Number of tokens to be upgraded (in 18 decimals)
     *
     * NOTE: It will use ´transferFrom´ to get tokens. Before calling this
     * function you should ´approve´ this contract
     */
    function upgrade(uint256 amount) external virtual;

    /**
     * @dev Token upgrade event
     * @param account Account whose tokens are upgraded
     * @param amount Amount of tokens upgraded (in 18 decimals)
     */
    event TokenUpgraded(
        address indexed account,
        uint256 amount
    );

    /**
     * @dev Downgrade SuperToken to ERC20.
     * @dev It will call transfer to send tokens
     * @param amount Number of tokens to be downgraded
     */
    function downgrade(uint256 amount) external virtual;

    /**
     * @dev Token downgrade event
     * @param account Account whose tokens are upgraded
     * @param amount Amount of tokens downgraded
     */
    event TokenDowngraded(
        address indexed account,
        uint256 amount
    );

    /**************************************************************************
     * Superfluid Batch Operations
     *************************************************************************/

    /**
     * @dev Perform ERC20 approve by host contract.
     * @param account The account owner to be approved.
     * @param spender The spender of account owner's funds.
     * @param amount Number of tokens to be approved.
     */
    function operationApprove(
        address account,
        address spender,
        uint256 amount
    ) external virtual;

    /**
     * @dev Perform ERC20 transfer from by host contract.
     * @param account The account to spend sender's funds.
     * @param sender  The account where the funds is sent from.
     * @param recipient The recipient of thefunds.
     * @param amount Number of tokens to be transferred.
     */
    function operationTransferFrom(
        address account,
        address sender,
        address recipient,
        uint256 amount
    ) external virtual;

    /**
     * @dev Upgrade ERC20 to SuperToken by host contract.
     * @param account The account to be changed.
     * @param amount Number of tokens to be upgraded (in 18 decimals)
     */
    function operationUpgrade(address account, uint256 amount) external virtual;

    /**
     * @dev Downgrade ERC20 to SuperToken by host contract.
     * @param account The account to be changed.
     * @param amount Number of tokens to be downgraded (in 18 decimals)
     */
    function operationDowngrade(address account, uint256 amount) external virtual;

    /**************************************************************************
    * System functions
    *************************************************************************/

    /**
     * @dev Get the contract address that is hosting this token.
     * @return host Superfluid host contract address
     */
    function getHost() external view virtual returns(address host);

    /**************************************************************************
     * Function modifiers for access control and parameter validations
     *
     * While they cannot be explicitly stated in function definitions, they are
     * listed in function definition comments instead for clarity.
     *
     * TODO: turning these off because solidity-coverage don't like it
     *************************************************************************/

    /* /// @dev The msg.sender must be a listed agreement.
    modifier onlyAgreement() virtual; */

}
