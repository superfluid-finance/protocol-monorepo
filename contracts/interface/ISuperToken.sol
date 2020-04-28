pragma solidity >= 0.6.6;

/**
 * @title Superfluid's token interface
 * @author Superfluid
 */
interface ISuperToken {

    function getState(
        address agreementClass,
        address account
    )
        external
        view
        returns (bytes memory state);

    function updateState(
        address sender,
        address receiver,
        bytes calldata senderState,
        bytes calldata receiverState
    ) external;

    function upgrade(uint256 amount) external;

    function downgrade(uint256 amount) external;

    function currentState(
        address sender,
        address receiver
    )
        external
        view
        returns(bytes memory state);

    function getAccountRateFlows(
        address account
    )
        external
        view
        returns(int256 creditor, int256 debitor);
}
