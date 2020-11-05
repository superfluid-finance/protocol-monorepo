const { shouldBehaveLikeCFAv1 } = require("./ConstantFlowAgreementV1.behavior.js");

const TestEnvironment = require("../../TestEnvironment");


contract("Using ConstantFlowAgreement v1", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 5));

    before(async () => {
        await t.reset();
    });

    beforeEach(async function () {
        await t.createNewToken();
    });

    context("#1 without callbacks", () => {
        shouldBehaveLikeCFAv1({
            prefix: "#1.1",
            testenv: t
        });
    });

});

// FIXME deprecated
require("./ConstantFlowAgreementV1.backup.test.js");
