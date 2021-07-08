// SPDX-License-Identifier: AGPLv3
pragma abicoder v2;
pragma solidity 0.7.6;

import { IResolver } from "../interfaces/misc/IResolver.sol";
import {
    ISuperfluid,
    ISuperTokenFactory,
    ISuperAgreement
} from "../interfaces/superfluid/ISuperfluid.sol";
import { Strings } from "../utils/Strings.sol";


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

    /* function loadToken(string memory tokenNameOrAddress)
        public view
    {

    } */
}
