// SPDX-License-Identifier: AGPLv3
pragma abicoder v2;
pragma solidity 0.7.6;

import { IResolver } from "../interfaces/ux/IResolver.sol";
import {
    ISuperfluid,
    ISuperTokenFactory,
    ISuperAgreement
} from "../interfaces/superfluid/ISuperfluid.sol";

import { Strings } from "../libs/Strings.sol";


/**
 * @dev Superfluid Loader
 *
 * A on-chain utility contract for loading framework objects in one view function.
 *
 * NOTE:
 * Q: Why don't we just use https://www.npmjs.com/package/ethereum-multicall?
 * A: Well, no strong reason other than also allowing on-chain one view function loading.
 */
contract SuperfluidLoader {

    IResolver private immutable _resolver;

    struct Framework {
        ISuperfluid superfluid;
        ISuperTokenFactory superTokenFactory;
        ISuperAgreement agreementCFAv1;
        ISuperAgreement agreementIDAv1;
    }

    constructor(IResolver resolver) {
        _resolver = resolver;
    }

    /**
     * @dev Load framework objects
     * @param releaseVersion Protocol release version of the deployment
     */
    function loadFramework(string calldata releaseVersion)
        external view
        returns (Framework memory result)
    {
        // load superfluid host contract
        result.superfluid = ISuperfluid(_resolver.get(
            Strings.concat(Strings.toSlice("Superfluid."), Strings.toSlice(releaseVersion))
        ));
        result.superTokenFactory = result.superfluid.getSuperTokenFactory();
        result.agreementCFAv1 = result.superfluid.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        );
        result.agreementIDAv1 = result.superfluid.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1")
        );
    }
}
