const SuperTokenStorageTester = artifacts.require("SuperTokenStorageTester");

contract("SuperTokenStorage", () => {
    it("#0 validate immutable storage layout", async () => {
        const tester = await SuperTokenStorageTester.new();
        await tester.validate.call();
    });
});
