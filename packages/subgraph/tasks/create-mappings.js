const { exec } = require("child_process");

const networks = [
    "ganache",
    "goerli",
    "kovan",
    "matic",
    "mumbai",
    "rinkeby",
    "ropsten",
    "xdai",
];
const handleErrors = (err, _stdout, stderr) => {
    if (err) {
        console.log("Error: " + err);
    }

    if (stderr) {
        console.log("Stderror:" + stderr);
    }
};
function main() {
    exec("mkdir tasks/config", () => {
        for (let i in networks) {
            const network = networks[i];
            const jsonConfigFilePath = `tasks/config/${network}.json`;
            exec(
                `jq -n '{ "network": "${network}", "NETWORK": "${network.toUpperCase()}" }' > ${jsonConfigFilePath}`,
                handleErrors
            );
            exec(
                `npx mustache ${jsonConfigFilePath} src/mappings/cfav1/cfav1.template 
				> src/mappings/cfav1/${network}CFAv1.ts`,
                handleErrors
            );
            exec(
                `npx mustache ${jsonConfigFilePath} src/mappings/idav1/idav1.template 
				> src/mappings/idav1/${network}IDAv1.ts`,
                handleErrors
            );
            exec(
                `npx mustache ${jsonConfigFilePath} src/mappings/superToken/superToken.template 
				> src/mappings/superToken/${network}SuperToken.ts`,
                handleErrors
            );
            exec(
                `npx mustache ${jsonConfigFilePath} src/mappings/superTokenFactory/superTokenFactory.template 
				> src/mappings/superTokenFactory/${network}SuperTokenFactory.ts`,
                handleErrors
            );
        }
    });
    return true;
}
main();
