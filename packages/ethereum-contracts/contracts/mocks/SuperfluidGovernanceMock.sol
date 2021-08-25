// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperfluid
} from "../interfaces/superfluid/ISuperfluid.sol";

import { SuperfluidGovernanceBase } from "../gov/SuperfluidGovernanceBase.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";

contract SuperfluidGovernanceUpgradabilityTester is SuperfluidGovernanceBase {

    // TODO: explicit constructor not needed bcs without args, correct?

    // @dev Make sure the storage layout never change over the course of the development
    function validateStorageLayout() external pure {
        uint256 slot;
        uint256 offset;

        // Initializable _initialized and _initialized

        // SuperfluidToken storages

        assembly { slot:= _configs.slot offset := _configs.offset }
        require (slot == 1 && offset == 0, "_configs changed location");
        // TODO: guess: 0.0 for bool Initializable._initialized, 0.1 for bool Initializable._initializing
        // ... aligned to 1.0 bcs mapping requires a full slot
    }

    function _requireAuthorised(ISuperfluid host) internal view override { }
}
