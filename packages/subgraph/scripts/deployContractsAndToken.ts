import { ethers } from "hardhat";
import deployFramework from "@superfluid-finance/ethereum-contracts/scripts/deploy-framework";
import deployTestToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-token";
import deploySuperToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-super-token";
import set3PsConfig from "@superfluid-finance/ethereum-contracts/scripts/gov-set-3Ps-config";

export const errorHandler = (type: string, err: any) => {
    if (err) console.error("Deploy " + type + " Error: ", err);
};

async function main() {
    const [Deployer] = (await ethers.getSigners()).map((x) => x.address);
    await deployFramework((x: any) => errorHandler("Framework", x), {
        web3: (global as any).web3,
        from: Deployer,
    });
    await deployTestToken(
        (x: any) => errorHandler("TestToken", x),
        [":", "fDAI"],
        {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
    await deploySuperToken(
        (x: any) => errorHandler("SuperToken", x),
        [":", "fDAI"],
        {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
    await set3PsConfig(
        (x: any) => errorHandler("3PsConfig", x),
        [":", "0x1f65B7b9b3ADB4354fF76fD0582bB6b0d046a41c", 3600, 720], {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
}

main()
    .then(() => {
        process.exit(0);
    })
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
