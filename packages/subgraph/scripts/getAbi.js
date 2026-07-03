const fs = require("fs");
const path = require("path");

const contracts = [
    "ERC20",
    "IConstantFlowAgreementV1",
    "ISuperTokenFactory",
    "ISuperToken",
    "ISuperfluid",
    "Resolver",
    "IInstantDistributionAgreementV1",
    "IGeneralDistributionAgreementV1",
    "ISuperfluidPool",
    "SuperfluidGovernanceBase",
    "TestToken",
    "TOGA",
];

const hardhatDir = path.join(__dirname, "../../ethereum-contracts/build/hardhat");

function indexHardhatArtifacts(dir) {
    const index = {};
    const walk = (currentDir) => {
        for (const ent of fs.readdirSync(currentDir, { withFileTypes: true })) {
            const entryPath = path.join(currentDir, ent.name);
            if (ent.isDirectory()) {
                walk(entryPath);
            } else if (ent.name.endsWith(".json") && !ent.name.endsWith(".dbg.json")) {
                const artifact = require(entryPath);
                if (artifact.contractName && artifact.abi) {
                    index[artifact.contractName] = artifact.abi;
                }
            }
        }
    };
    walk(dir);
    return index;
}

if (!fs.existsSync(hardhatDir)) {
    console.error(`Missing ${hardhatDir}. Run yarn workspace @superfluid-finance/ethereum-contracts build first.`);
    process.exit(1);
}

const artifactsByName = indexHardhatArtifacts(hardhatDir);

fs.mkdirSync("abis", { recursive: true });
console.log("Fetched ABIs for the following contracts:");

let missing = 0;
for (const contractName of contracts) {
    const abi = artifactsByName[contractName];
    if (!abi) {
        console.error(`- ${contractName} (missing)`);
        missing++;
        continue;
    }
    fs.writeFileSync(path.join("abis", `${contractName}.json`), JSON.stringify(abi));
    console.log(`- ${contractName}`);
}

if (missing > 0) {
    process.exit(1);
}
