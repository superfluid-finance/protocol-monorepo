// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.23;

import { ISuperfluid, BatchOperation } from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { IConstantFlowAgreementV1 } from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { MacroForwarder, IUserDefinedMacro } from "../../../contracts/utils/MacroForwarder.sol";
import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";

using SuperTokenV1Library for ISuperToken;

contract NaugthyMacro {
    int naughtyCounter = -1;

    constructor(bool beNaughty) {
        if (beNaughty) naughtyCounter = 0;
    }

    // if naughtyCounter >= 0, this changes state, which leads to a rever in the context of a macro call
    function buildBatchOperations(ISuperfluid, bytes memory, address /*msgSender*/) external
        returns (ISuperfluid.Operation[] memory /*operation*/)
    {
        // Do the naughty thing (updating state as an expected view function)
        if (naughtyCounter >= 0) {
            naughtyCounter++;
        }
        return new ISuperfluid.Operation[](0);
    }
}

contract GoodMacro is IUserDefinedMacro {
    function buildBatchOperations(ISuperfluid host, bytes memory params, address /*msgSender*/) external override view
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

// deletes a bunch of flows of the msgSender
contract MultiFlowDeleteMacro is IUserDefinedMacro {
    function buildBatchOperations(ISuperfluid host, bytes memory params, address msgSender) external override view
        returns (ISuperfluid.Operation[] memory operations)
    {
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(address(host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));

        // parse params
        (ISuperToken token, address[] memory receivers) =
            abi.decode(params, (ISuperToken, address[]));

        // construct batch operations
        operations = new ISuperfluid.Operation[](receivers.length);
        for (uint i = 0; i < receivers.length; ++i) {
            bytes memory callData = abi.encodeCall(cfa.deleteFlow,
                                                   (token,
                                                    msgSender,
                                                    receivers[i],
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

/*
 * Example for a macro which has all the state needed, thus needs no additional calldata
 * in the context of batch calls.
 * Important: state changes do NOT take place in the context of macro calls.
 */
contract StatefulMacro is IUserDefinedMacro {
    struct Config {
        MacroForwarder macroForwarder;
        ISuperToken superToken;
        int96 flowRate;
        address[] recipients;
        address referrer;
    }
    Config public config;

    // imagine this to be permissioned, e.g. using Ownable
    function setConfig(Config memory config_) public {
        config = config_;
    }

    function buildBatchOperations(ISuperfluid host, bytes memory /*params*/, address /*msgSender*/)
        external override view
        returns (ISuperfluid.Operation[] memory operations)
    {
        // host-agnostic deployment. alternatively, you may hard code cfa too
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(address(host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));

        // construct batch operations from persisted config
        operations = new ISuperfluid.Operation[](config.recipients.length);
        for (uint i = 0; i < config.recipients.length; ++i) {
            bytes memory callData = abi.encodeCall(cfa.createFlow,
                                                   (config.superToken,
                                                    config.recipients[i],
                                                    config.flowRate,
                                                    new bytes(0) // placeholder
                                                   ));
            operations[i] = ISuperfluid.Operation({
                operationType : BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
                target: address(cfa),
                data: abi.encode(callData, abi.encode(config.referrer))
            });
        }
    }
}

contract MacroForwarderTest is FoundrySuperfluidTester {
    MacroForwarder internal macroForwarder;

    constructor() FoundrySuperfluidTester(5) {
    }

    function setUp() public override {
        super.setUp();
        macroForwarder = new MacroForwarder(sf.host);
        vm.startPrank(address(sf.governance.owner()));
        sf.governance.enableTrustedForwarder(sf.host, ISuperToken(address(0)), address(macroForwarder));
        vm.stopPrank();
    }

    function testDummyMacro() external {
        NaugthyMacro m = new NaugthyMacro(false /* not naughty */);
        macroForwarder.runMacro(IUserDefinedMacro(address(m)), new bytes(0));
    }

    function testNaugtyMacro() external {
        NaugthyMacro m = new NaugthyMacro(true /* naughty */);
        vm.expectRevert();
        // Note: need to cast the naughty macro
        macroForwarder.runMacro(IUserDefinedMacro(address(m)), new bytes(0));
    }

    function testGoodMacro() external {
        GoodMacro m = new GoodMacro();
        address[] memory recipients = new address[](2);
        recipients[0] = bob;
        recipients[1] = carol;
        vm.startPrank(admin);
        // NOTE! This is different from abi.encode(superToken, int96(42), [bob, carol]),
        //       which is a fixed array: address[2].
        macroForwarder.runMacro(m, abi.encode(superToken, int96(42), recipients));
        assertEq(sf.cfa.getNetFlow(superToken, bob), 42);
        assertEq(sf.cfa.getNetFlow(superToken, carol), 42);
        vm.stopPrank();
    }

    function testStatefulMacro() external {
        address[] memory recipients = new address[](2);
        recipients[0] = bob;
        recipients[1] = carol;
        StatefulMacro m = new StatefulMacro();
        m.setConfig(StatefulMacro.Config(
                macroForwarder, superToken, 42, recipients, dan
        ));
        vm.startPrank(admin);
        macroForwarder.runMacro(m, new bytes(0));
        assertEq(sf.cfa.getNetFlow(superToken, bob), 42);
        assertEq(sf.cfa.getNetFlow(superToken, carol), 42);
        vm.stopPrank();
    }

    function testMultiFlowDeleteMacro() external {
        MultiFlowDeleteMacro m = new MultiFlowDeleteMacro();
        address[] memory recipients = new address[](3);
        recipients[0] = bob;
        recipients[1] = carol;
        recipients[2] = dan;

        vm.startPrank(admin);
        // flows to be deleted need to exist in the first place
        for (uint i = 0; i < recipients.length; ++i) {
            superToken.createFlow(recipients[i], 42);
        }
        // now batch-delete them
        macroForwarder.runMacro(m, abi.encode(superToken, recipients));

        for (uint i = 0; i < recipients.length; ++i) {
            assertEq(sf.cfa.getNetFlow(superToken, recipients[i]), 0);
        }
        vm.stopPrank();
    }
}
