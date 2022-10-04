import { ethers } from "hardhat";
import { deployTestFramework } from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework";

export const errorHandler = (type: string, err: any) => {
    if (err) console.error("Deploy " + type + " Error: ", err);
};

export async function deployContractsAndToken() {
    const [Deployer] = await ethers.getSigners();

    const deployer = await deployTestFramework();

    console.log("Deploying Wrapper Super Token...");
    await deployer
        .connect(Deployer)
        .deployWrapperSuperToken(
            "Fake DAI",
            "fDAI",
            18,
            ethers.utils.parseUnits("1000000000000")
        );

    console.log("Deploying Native Asset Super Token...");
    await deployer
        .connect(Deployer)
        .deployNativeAssetSuperToken("Super ETH", "ETHx");

    console.log("Deploying Pure Super Token...");
    await deployer
        .connect(Deployer)
        .deployPureSuperToken(
            "Mr.Token",
            "MRx",
            ethers.utils.parseUnits("1000000000000")
        );
    return deployer;
}
