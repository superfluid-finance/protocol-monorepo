// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { ISuperToken } from "../interfaces/superfluid/ISuperToken.sol";
import { IConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { IConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";

library SuperfluidNFTDeployerLibrary {
    /**
     * @notice Deploys the NFT proxy contracts and initializes them
     * @dev This does not link the proxies to the SuperToken.
     * @param superToken the super token we are attaching the NFT contracts to
     * @param constantOutflowNFTLogic address of the constant outflow NFT logic contract
     * @param constantInflowNFTLogic address of the constant inflow NFT logic contract
     * @return constantOutflowNFTProxyAddress the deployed constant outflow NFT contract
     * @return constantInflowNFTProxyAddress the deployed constant inflow NFT contract
     */
    function deployNFTProxyContractsAndInitialize(
        ISuperToken superToken,
        address constantOutflowNFTLogic,
        address constantInflowNFTLogic
    )
        external
        returns (
            address constantOutflowNFTProxyAddress,
            address constantInflowNFTProxyAddress
        )
    {
        string memory superTokenSymbol = superToken.symbol();
        UUPSProxy constantOutflowNFTProxy = new UUPSProxy();
        UUPSProxy constantInflowNFTProxy = new UUPSProxy();

        constantOutflowNFTProxy.initializeProxy(constantOutflowNFTLogic);
        constantInflowNFTProxy.initializeProxy(constantInflowNFTLogic);

        IConstantOutflowNFT(address(constantOutflowNFTProxy)).initialize(
            superToken,
            string.concat(superTokenSymbol, " Outflow NFT"),
            string.concat(superTokenSymbol, "COF")
        );
        IConstantInflowNFT(address(constantInflowNFTProxy)).initialize(
            superToken,
            string.concat(superTokenSymbol, " Inflow NFT"),
            string.concat(superTokenSymbol, "CIF")
        );

        constantOutflowNFTProxyAddress = address(constantOutflowNFTProxy);
        constantInflowNFTProxyAddress = address(constantInflowNFTProxy);
    }
}
