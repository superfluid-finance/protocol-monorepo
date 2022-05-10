import { ethers } from "hardhat";
import deployFramework from "@superfluid-finance/ethereum-contracts/scripts/deploy-framework";
import deployTestToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-token";
import deploySuperToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-super-token";

export const errorHandler = (type: string, err: any) => {
    if (err) console.error("Deploy " + type + " Error: ", err);
};

export async function deployFrameworkAndTokens() {
    try {
        const [Deployer] = (await ethers.getSigners()).map(x => x.address);
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
            },
        );
        await deploySuperToken(
            (x: any) => errorHandler("SuperToken", x),
            [":", "fDAI"],
            {
                web3: (global as any).web3,
                from: Deployer,
            },
        );
    } catch (err) {
        console.error(err);
    }
}
