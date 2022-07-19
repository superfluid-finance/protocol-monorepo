const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const AgreementForwarder = artifacts.require("AgreementForwarder");

const mintAmount = "1000000000000000000000000000"; // a small loan of a billion dollars
const flowRate = "1000000000000";

describe("Agreement Forwarder", function () {
    const t = TestEnvironment.getSingleton();
    const {ZERO_ADDRESS} = t.constants;
    let superToken, host, cfa, governance, agrFwd;
    let alice, bob;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 3,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;
        governance = t.contracts.governance;

        agrFwd = await AgreementForwarder.new(host.address);

        await governance.enableTrustedForwarder(
            host.address,
            ZERO_ADDRESS,
            agrFwd.address
        );

        ({alice, bob} = t.aliases);
    });

    beforeEach(async () => {
        superToken = await SuperTokenMock.new(host.address, "69");
        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob, mintAmount, "0x", "0x");
    });

    describe("Constant Flow Ops", async function () {
        it("Provide correct error", async () => {
            await expectRevertedWith(
                agrFwd.createConstantFlow(superToken.address, bob, 0, {
                    from: alice,
                }),
                "CFA: invalid flow rate"
            );
        });

        it("Create Constant Flow", async () => {
            await agrFwd.createConstantFlow(superToken.address, bob, flowRate, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate
            );
        });
    });
});
