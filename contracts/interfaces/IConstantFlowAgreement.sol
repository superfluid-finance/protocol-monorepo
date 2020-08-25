// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import "./ISuperToken.sol";

interface IConstantFlowAgreement {

    function createFlow(ISuperToken token, address sender, address to, int256 flowRate) external;

    function getFlow(ISuperToken token, address sender, address receiver) external returns(int256);

    function getFlow(
       ISuperToken token,
       bytes32 flowId
    )
        external
        view
        returns (
            uint256 timestamp,
            address sender,
            address receiver,
            int256 flowRate
        );

    function updateFlow(ISuperToken token, address sender, address to, int256 flowRate) external;

    function deleteFlow(ISuperToken token, address from, address to) external;
}
