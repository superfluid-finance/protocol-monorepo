// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { ISuperfluid, BatchOperation } from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { IConstantFlowAgreementV1 } from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { TrustedMacrosVanilla, IUserDefinedMacro } from "../../../contracts/utils/TrustedMacrosVanilla.sol";
import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";


contract DummyMacro is IUserDefinedMacro {
    function executeMacro(ISuperfluid, bytes memory) override external pure
        returns (ISuperfluid.Operation[] memory operations)
    {
        operations = new ISuperfluid.Operation[](0);
    }
}

contract NautyMacro {
    uint naughtyCounter;

    function executeMacro(ISuperfluid, bytes memory) external
        returns (ISuperfluid.Operation[] memory operation)
    {
        naughtyCounter++;
    }
}

contract GoodMacro is IUserDefinedMacro {
    function executeMacro(ISuperfluid host, bytes memory params) external view
        returns (ISuperfluid.Operation[] memory operations)
    {
        // host-agnostic deployment. alternatively, you may hard code cfa too
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(address(host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
        // parse params
        (ISuperToken token, int96 flowRate, address[] memory recipients) =
            abi.decode(params, (ISuperToken, int96, address[]));
        // construct batch operations
        operations = new ISuperfluid.Operation[](recipients.length);
        // Build batch call operations here
        for (uint i = 0; i < recipients.length; ++i) {
            bytes memory callData = abi.encodeCall(cfa.createFlow,
                                                   (token,
                                                    recipients[i],
                                                    flowRate,
                                                    new bytes(0) // placeholder
                                                   ));
            operations[i] = ISuperfluid.Operation({
                operationType : BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
                target: address(cfa),
                data: abi.encode(callData, new bytes(0))
                });
        }
    }
}

contract TrustedMacrosVanillaTest is FoundrySuperfluidTester {
    TrustedMacrosVanilla internal trustedMacros;

    constructor() FoundrySuperfluidTester(5) {
    }

    function setUp() public override {
        super.setUp();
        trustedMacros = new TrustedMacrosVanilla(sf.host);
        vm.startPrank(address(sf.governance.owner()));
        sf.governance.enableTrustedForwarder(sf.host, ISuperToken(address(0)), address(trustedMacros));
        vm.stopPrank();
    }

    function testDummyMacro() external {
        DummyMacro m = new DummyMacro();
        trustedMacros.runMacro(m, new bytes(0));
    }

    function testNaugtyMacro() external {
        NautyMacro m = new NautyMacro();
        vm.expectRevert();
        // Note: need to cast the naughty macro
        trustedMacros.runMacro(IUserDefinedMacro(address(m)), new bytes(0));
    }

    function testGoodMacro() external {
        GoodMacro m = new GoodMacro();
        address[] memory recipients = new address[](2);
        recipients[0] = bob;
        recipients[1] = carol;
        vm.startPrank(admin);
        // NOTE! This is different from abi.encode(superToken, int96(42), [bob, carol]),
        //       which is a fixed array: address[2].
        trustedMacros.runMacro(m, abi.encode(superToken, int96(42), recipients));
        assertEq(sf.cfa.getNetFlow(superToken, bob), 42);
        assertEq(sf.cfa.getNetFlow(superToken, carol), 42);
        vm.stopPrank();
    }
}
