// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";

library SuperfluidNFTDeployerLibrary {
    function deployConstantOutflowNFT() external returns (address) {
        return address(new ConstantOutflowNFT());
    }

    function deployConstantInflowNFT() external returns (address) {
        return address(new ConstantInflowNFT());
    }
}
