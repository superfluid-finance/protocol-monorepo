// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { TokenInfo } from "../tokens/TokenInfo.sol";
import { IERC777 } from "@openzeppelin/contracts/token/ERC777/IERC777.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/**
 * @title Superfluid's token interface.
 *
 * @author Superfluid
 */
interface ISuperToken is TokenInfo, IERC20, IERC777 {

    /**************************************************************************
    * TokenInfo & ERC777
    *************************************************************************/

    /**
     * @dev Returns the name of the token.
     */
    function name() external view override(IERC777, TokenInfo) returns (string memory);

    /**
     * @dev Returns the symbol of the token, usually a shorter version of the
     * name.
     */
    function symbol() external view override(IERC777, TokenInfo) returns (string memory);



    /**
     * @dev Returns the number of decimals used to get its user representation.
     * For example, if `decimals` equals `2`, a balance of `505` tokens should
     * be displayed to a user as `5,05` (`505 / 10 ** 2`).
     *
     * Tokens usually opt for a value of 18, imitating the relationship between
     * Ether and Wei. This is the value {ERC20} uses, unless {_setupDecimals} is
     * called.
     *
     * NOTE: SuperToken always uses 18 decimals.
     *
     * Note: This information is only used for _display_ purposes: it in
     * no way affects any of the arithmetic of the contract, including
     * {IERC20-balanceOf} and {IERC20-transfer}.
     */
    function decimals() external view override(TokenInfo) returns (uint8);

    /**************************************************************************
    * ERC20 & ERC777
    *************************************************************************/

    /**
     * @dev See {IERC20-totalSupply}.
     */
    function totalSupply() external view override(IERC777, IERC20) returns (uint256);

    /**
     * @dev Returns the amount of tokens owned by an account (`owner`).
     */
    function balanceOf(address account) external view override(IERC777, IERC20) returns(uint256 balance);

    /**************************************************************************
    * ERC20
    *************************************************************************/

    /**
     * @dev Moves `amount` tokens from the caller's account to `recipient`.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transfer(address recipient, uint256 amount) external override(IERC20) returns (bool);

    /**
     * @dev Returns the remaining number of tokens that `spender` will be
     * allowed to spend on behalf of `owner` through {transferFrom}. This is
     * zero by default.
     *
     * This value changes when {approve} or {transferFrom} are called.
     */
    function allowance(address owner, address spender) external override(IERC20) view returns (uint256);

    /**
     * @dev Sets `amount` as the allowance of `spender` over the caller's tokens.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * IMPORTANT: Beware that changing an allowance with this method brings the risk
     * that someone may use both the old and the new allowance by unfortunate
     * transaction ordering. One possible solution to mitigate this race
     * condition is to first reduce the spender's allowance to 0 and set the
     * desired value afterwards:
     * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
     *
     * Emits an {Approval} event.
     */
    function approve(address spender, uint256 amount) external override(IERC20) returns (bool);

    /**
     * @dev Moves `amount` tokens from `sender` to `recipient` using the
     * allowance mechanism. `amount` is then deducted from the caller's
     * allowance.
     *
     * Returns a boolean value indicating whether the operation succeeded.
     *
     * Emits a {Transfer} event.
     */
    function transferFrom(address sender, address recipient, uint256 amount) external override(IERC20) returns (bool);

    /**************************************************************************
    * ERC777
    *************************************************************************/

    function granularity() external view override(IERC777) returns (uint256);
    function send(address recipient, uint256 amount, bytes calldata data) external override(IERC777);
    function burn(uint256 amount, bytes calldata data) external override(IERC777);
    function isOperatorFor(address operator, address tokenHolder) external override(IERC777) view returns (bool);
    function authorizeOperator(address operator) external override(IERC777);
    function revokeOperator(address operator) external override(IERC777);
    function defaultOperators() external override(IERC777) view returns (address[] memory);
    function operatorSend(
        address sender,
        address recipient,
        uint256 amount,
        bytes calldata data,
        bytes calldata operatorData
    ) external override(IERC777);
    function operatorBurn(
        address account,
        uint256 amount,
        bytes calldata data,
        bytes calldata operatorData
    ) external override(IERC777);

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
        external;

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
        external view
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
        external;

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
        external;

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
        external;

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
        external;

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
        external view
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
        external;

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

        view
        returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit);

    /// @dev realtimeBalanceOf with timestamp equals to block.timestamp
    function realtimeBalanceOfNow(
        address account
    )
        external

        view
        returns (int256 availableBalance, uint256 deposit, uint256 owedDeposit);

    function transferAll(address recipient)
        external
        ;

    /**************************************************************************
     * ERC20 wrapping
     *************************************************************************/

    /**
     * @dev Return the underlaying token contract
     * @return tokenAddr Underlying token address
     */
    function getUnderlayingToken() external view returns(address tokenAddr);

    /**
     * @dev Upgrade ERC20 to SuperToken.
     * @param amount Number of tokens to be upgraded (in 18 decimals)
     *
     * NOTE: It will use ´transferFrom´ to get tokens. Before calling this
     * function you should ´approve´ this contract
     */
    function upgrade(uint256 amount) external;

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
    function downgrade(uint256 amount) external;

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
    ) external;

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
    ) external;

    /**
     * @dev Upgrade ERC20 to SuperToken by host contract.
     * @param account The account to be changed.
     * @param amount Number of tokens to be upgraded (in 18 decimals)
     */
    function operationUpgrade(address account, uint256 amount) external;

    /**
     * @dev Downgrade ERC20 to SuperToken by host contract.
     * @param account The account to be changed.
     * @param amount Number of tokens to be downgraded (in 18 decimals)
     */
    function operationDowngrade(address account, uint256 amount) external;

    /**************************************************************************
    * System functions
    *************************************************************************/

    /**
     * @dev Get the contract address that is hosting this token.
     * @return host Superfluid host contract address
     */
    function getHost() external view returns(address host);

    /**************************************************************************
     * Function modifiers for access control and parameter validations
     *
     * While they cannot be explicitly stated in function definitions, they are
     * listed in function definition comments instead for clarity.
     *
     * TODO: turning these off because solidity-coverage don't like it
     *************************************************************************/

    /* /// @dev The msg.sender must be a listed agreement.
    modifier onlyAgreement() ; */

}
