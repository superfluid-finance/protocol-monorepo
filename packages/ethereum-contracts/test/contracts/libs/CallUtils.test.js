const {ethers} = require("hardhat");
const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");

describe("CallUtils", function () {
    const t = TestEnvironment.getSingleton();

    let callUtilsMock;
    let callUtilsTester;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 5,
        });
        callUtilsMock = await ethers.getContractFactory("CallUtilsMock");
        callUtilsMock = await callUtilsMock.deploy();

        callUtilsTester = await ethers.getContractFactory("CallUtilsTester");
        callUtilsTester = await callUtilsTester.deploy();
    });

    it("CallUtils.revertFromReturnedData", async () => {
        await expectRevertedWith(
            callUtilsMock.revertTest("revertEmpty()"),
            "CallUtils: target revert()"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertAssert()"),
            "CallUtils: target panicked: 0x01"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertOverflow()"),
            "CallUtils: target panicked: 0x11"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertDivByZero()"),
            "CallUtils: target panicked: 0x12"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertEnum()"),
            "CallUtils: target panicked: 0x21"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertPop()"),
            "CallUtils: target panicked: 0x31"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertArrayAccess()"),
            "CallUtils: target panicked: 0x32"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertBigArray()"),
            "CallUtils: target panicked: 0x41"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertZeroInitializedFunctionPointer()"),
            "CallUtils: target panicked: 0x51"
        );

        await expectRevertedWith(
            callUtilsMock.revertTest("revertString()"),
            "gm"
        );
        // TODO: Add revert custom error tests
    });

    it("CallUtils.isValidAbiEncodedBytes", async () => {
        await callUtilsTester.testIsValidAbiEncodedBytes();
    });
});
