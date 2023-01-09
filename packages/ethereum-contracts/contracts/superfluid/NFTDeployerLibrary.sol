// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperfluid, ISuperToken } from "../superfluid/Superfluid.sol";
import { CFAOutflowNFT } from "./CFAOutflowNFT.sol";
import { CFAInflowNFT } from "./CFAInflowNFT.sol";

library NFTDeployerLibrary {
    function deployCFAOutflowNFT(
        ISuperfluid _host,
        ISuperToken _superToken,
        string memory _name,
        string memory _symbol
    ) external returns (address) {
        address cfaAddress = address(
            _host.getAgreementClass(
                keccak256(
                    "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                )
            )
        );
        return
            address(
                new CFAOutflowNFT(
                    _host,
                    _superToken,
                    IConstantFlowAgreementV1(cfaAddress),
                    string.concat(_symbol, "COF"),
                    string.concat(_name, " Outflow NFT")
                )
            );
    }

    function deployCFAInflowNFT(
        ISuperfluid _host,
        ISuperToken _superToken,
        string memory _name,
        string memory _symbol
    ) external returns (address) {
        address cfaAddress = address(
            _host.getAgreementClass(
                keccak256(
                    "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                )
            )
        );
        return
            address(
                new CFAInflowNFT(
                    _host,
                    _superToken,
                    IConstantFlowAgreementV1(cfaAddress),
                    string.concat(_symbol, "CIF"),
                    string.concat(_name, " Inflow NFT")
                )
            );
    }
}
