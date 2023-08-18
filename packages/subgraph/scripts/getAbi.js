const fs = require("fs");
const path = require("path");

const contracts = [
    "ConstantFlowAgreementV1",
    "ERC20",
    "IConstantFlowAgreementV1",
    "IResolver",
    "ISuperTokenFactory",
    "ISuperToken",
    "ISuperfluid",
    "Resolver",
    "IInstantDistributionAgreementV1",
    "InstantDistributionAgreementV1",
    "SuperfluidGovernanceBase",
    "SuperToken",
    "TestToken",
    "TOGA",
];

const directoryPath = path.join(
    __dirname,
    "../../ethereum-contracts/build/truffle"
);

fs.mkdir("abis/", (err) => {
    if (err) return; //console.error(err);
    console.log("abis/ directory created");
});

fs.readdir(directoryPath, (err, files) => {
    if (err) return console.log(err);
    console.log("Fetched ABIs for the following contracts:");
    files.forEach((fileName) => {
        const contractName = fileName.split(".")[0];
        if (!contracts.includes(contractName)) return;
        const data = require(path.join(directoryPath, fileName));
        const abi = data.abi;
        fs.writeFile(`abis/${fileName}`, JSON.stringify(abi), (err) => {
            if (err) throw err;
            console.log(`- ${contractName}`);
        });
    });
});
