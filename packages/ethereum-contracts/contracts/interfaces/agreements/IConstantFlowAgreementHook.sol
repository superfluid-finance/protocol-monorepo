// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ConstantFlowAgreementV1 } from "../../agreements/ConstantFlowAgreementV1.sol";
import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

interface IConstantFlowAgreementHook {
    function onCreate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external returns (bool);

    function onUpdate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external returns (bool);

    function onDelete(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external returns (bool);
}
