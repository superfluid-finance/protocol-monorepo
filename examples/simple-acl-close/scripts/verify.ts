import { HardhatRuntimeEnvironment } from "hardhat/types";

export async function verifyContract(
    hre: HardhatRuntimeEnvironment,
    address: string,
    constructorArguments: any[],
) {
    try {
        await hre.run("verify:verify", {
            address,
            constructorArguments,
        });
    } catch (err) {
        console.error(err);
    }
}
