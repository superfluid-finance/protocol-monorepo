import {ethers} from "hardhat";
import {CallUtilsMock, CallUtilsTester} from "../../../typechain-types";
const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

describe("CallUtils", function () {
    const t = TestEnvironment.getSingleton();

    let CallUtilsMock: CallUtilsMock;
    let CallUtilsTester: CallUtilsTester;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        const CallUtilsMockFactory = await ethers.getContractFactory(
            "CallUtilsMock"
        );
        CallUtilsMock = await CallUtilsMockFactory.deploy();

        const CallUtilsTesterFactory = await ethers.getContractFactory(
            "CallUtilsTester"
        );
        CallUtilsTester = await CallUtilsTesterFactory.deploy();
    });

    it("CallUtils.revertFromReturnedData", async () => {
        await expectRevertedWith(
            CallUtilsMock.revertTest("revertEmpty()"),
            "CallUtils: target revert()"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertAssert()"),
            "CallUtils: target panicked: 0x01"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertOverflow()"),
            "CallUtils: target panicked: 0x11"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertDivByZero()"),
            "CallUtils: target panicked: 0x12"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertEnum()"),
            "CallUtils: target panicked: 0x21"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertPop()"),
            "CallUtils: target panicked: 0x31"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertArrayAccess()"),
            "CallUtils: target panicked: 0x32"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertBigArray()"),
            "CallUtils: target panicked: 0x41"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertZeroInitializedFunctionPointer()"),
            "CallUtils: target panicked: 0x51"
        );

        await expectRevertedWith(
            CallUtilsMock.revertTest("revertString()"),
            "gm"
        );
    });

    it("CallUtils.isValidAbiEncodedBytes", async () => {
        await CallUtilsTester.testIsValidAbiEncodedBytes();
    });
});
