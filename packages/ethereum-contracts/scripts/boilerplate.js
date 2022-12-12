module.exports = function (filename, simpleMigrationText) {
    console.log(`[DEPRECATED] - scripts/${filename} \n`);
    console.log(
        `scripts/${filename} is deprecated, please use dev-scripts/deploy-test-framework.js instead.\n`
    );
    console.log(
        `NOTE: dev-scripts/deploy-test-framework.js is only compatible with Hardhat.`
    );
    console.log(
        `IF you are using truffle, please change the path from scripts/${filename} => ops-scripts/${filename}\n`
    );
    console.log(
        `You will need to do the same with any other scripts you were previously importing from scripts.\n`
    );

    console.log(
        `Migration for ${filename} for truffle based projects is complete now.\n\n`
    );

    if (simpleMigrationText) {
        console.log(simpleMigrationText);
    }

    console.log(`IF you are using Hardhat, please keep reading:\n`);
    console.log(
        `For a hands-off example of how to migrate: https://github.com/superfluid-finance/protocol-monorepo/commit/810e42362259ba6bd242a09857c154d280617469#diff-6d53716f733fb02100c28af8c55ebeb1f961e6e466d07451ee1c7af49b3122e2\n`
    );

    console.log(
        "For a more hands-on migration walk through, please continue reading:\n"
    );
    console.log(
        "The new dev-scripts file deploys SuperfluidFrameworkDeployer.sol contract which deploys Superfluid and is the entrypoint for token deployments.\n"
    );
};
