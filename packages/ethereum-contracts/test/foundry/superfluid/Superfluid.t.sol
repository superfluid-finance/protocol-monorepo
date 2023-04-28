// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";
import {
    ISuperAgreement
} from "../../../contracts/interfaces/superfluid/ISuperAgreement.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import { AgreementMock } from "../../../contracts/mocks/AgreementMock.sol";

contract SuperfluidIntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    uint32 private constant _MAX_NUM_AGREEMENTS = 256;
    uint32 private constant _NUM_AGREEMENTS = 2;

    constructor() FoundrySuperfluidTester(3) {}

    function testRevertRegisterMax256Agreements() public {
        ISuperAgreement[] memory mocks = new ISuperAgreement[](
            _MAX_NUM_AGREEMENTS
        );
        mocks[0] = ISuperAgreement(address(sf.cfa));
        mocks[1] = ISuperAgreement(address(sf.ida));
        for (uint256 i; i < _MAX_NUM_AGREEMENTS - _NUM_AGREEMENTS; ++i) {
            bytes32 id = keccak256(abi.encode("type.", i));
            AgreementMock mock = new AgreementMock(address(sf.host), id, i);

            vm.startPrank(sf.governance.owner());
            sf.governance.registerAgreementClass(sf.host, address(mock));
            vm.stopPrank();
            mocks[i + _NUM_AGREEMENTS] = ISuperAgreement(address(mock));
        }

        ISuperAgreement[] memory agreementClasses = sf.host.mapAgreementClasses(
            type(uint256).max
        );

        for (uint256 i; i < _MAX_NUM_AGREEMENTS; ++i) {
            assertEq(
                address(agreementClasses[i]),
                address(mocks[i]),
                "Superfluid.t: agreement class not registered"
            );
        }

        AgreementMock mock = new AgreementMock(
            address(sf.host),
            keccak256(abi.encode("max.bad")),
            _MAX_NUM_AGREEMENTS + 1
        );

        vm.startPrank(sf.governance.owner());
        vm.expectRevert(ISuperfluid.HOST_MAX_256_AGREEMENTS.selector);
        sf.governance.registerAgreementClass(sf.host, address(mock));
        vm.stopPrank();
    }
}
