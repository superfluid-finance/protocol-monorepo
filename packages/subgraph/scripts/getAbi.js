const fs = require("fs");
const path = require("path");

const contracts = [
    "IConstantFlowAgreementV1",
    "ISuperTokenFactory",
    "ISuperToken",
];

const directoryPath = path.join(
    __dirname,
    "../../ethereum-contracts/build/contracts"
);

fs.mkdir("abis/", err => {
    if (err) return; //console.error(err);
    console.log("abis/ directory created");
});

fs.readdir(directoryPath, (err, files) => {
    if (err) return console.log(err);
    console.log("Fetched ABIs for the following contracts:");
    files.forEach(fileName => {
        const contractName = fileName.split(".")[0];
        if (!contracts.includes(contractName)) return;
        const data = require(path.join(directoryPath, fileName));
        const abi = data.abi;
        fs.writeFile(`abis/${fileName}`, JSON.stringify(abi), err => {
            if (err) throw err;
            console.log(`- ${contractName}`);
        });
    });
});
