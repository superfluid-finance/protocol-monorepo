// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

interface IConstantFlowAgreement {

    function createFlow(address sender, address to, int256 flowRate) external;

    function getFlow(bytes32 agreementId) external returns(address, address, int256);

    function updateFlow(address sender, address to, int256 flowRate) external;

    function deleteFlow(address from, address to) external;
}
