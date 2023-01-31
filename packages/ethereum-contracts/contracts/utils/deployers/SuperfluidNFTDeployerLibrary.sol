// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ConstantOutflowNFT } from "../../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../../superfluid/ConstantInflowNFT.sol";

library SuperfluidNFTDeployerLibrary {

    /// @notice Deploys the Superfluid ConstantOutflowNFT Contract
    /// @return constantOutflowNFT newly deployed ConstantOutflowNFT contract
    function deployConstantOutflowNFT()
        public
        returns (ConstantOutflowNFT constantOutflowNFT)
    {
        constantOutflowNFT = new ConstantOutflowNFT();
    }

    /// @notice Deploys the Superfluid ConstantInflowNFT Contract
    /// @return constantInflowNFT newly deployed ConstantInflowNFT contract
    function deployConstantInflowNFT()
        public
        returns (ConstantInflowNFT constantInflowNFT)
    {
        constantInflowNFT = new ConstantInflowNFT();
    }
}
