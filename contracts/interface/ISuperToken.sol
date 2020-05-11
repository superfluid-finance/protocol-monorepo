pragma solidity >= 0.6.6;

/**
 * @title Superfluid's token interface
 * @author Superfluid
 */
interface ISuperToken {

    /// @notice Register or update a agreement TODO fix comments
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function createAgreement(
        address agreementClass,
        bytes32 id,
        bytes data,
    ) external;

    /// @notice Register or update a agreement TODO fix comments
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        view
        returns(bytes memory state);

    /// @notice Register or update a agreement TODO fix comments
    /// @param agreementClass Contract address of the agreement
    /// @param id Agreement ID
    function terminateAgreement(
        address agreementClass,
        bytes32 id
    ) external;

    /// @notice ... TODO fix comments
    /// @param account Account of the agrement
    /// @param state Agreement state of the account
    function updateAgreementState(
        address agreementClass,
        bytes account,
        bytes calldata state
    ) external;

    /// @notice ... TODO fix comments
    /// @param agreementClass Contract address of the agreement
    /// @param account Account of the agrement
    function getAgreementState(
        address agreementClass,
        bytes account
    )
        external
        view
        returns (bytes memory data);

    /// @notice Upgrade ERC20 to SuperToken.
    /// @dev This method will ´transferFrom´ the tokens. Before calling this
    ///      function you should ´approve´ this contract
    /// @param amount Number of tokens to be upgraded
    function upgrade(uint256 amount) external;

    /// @notice Downgrade SuperToken to ERC20.
    /// @dev TODO ....
    /// @param amount Number of tokens to be downgraded
    function downgrade(uint256 amount) external;

}
