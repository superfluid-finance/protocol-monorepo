import { ethers } from "hardhat";
import SuperfluidSDK from "@superfluid-finance/js-sdk";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";

// the resolver address should be consistent as long as you use the 
// first account retrieved by hardhat's ethers.getSigners():
// 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266 and the nonce is 0
const RESOLVER_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

/**************************************************************************
 * Test Helper Functions
 *************************************************************************/
/**
 * Before setup function - deploy the framework, get signers, deploy test tokens,
 * initialize framework.
 * @param userAddresses
 * @returns
 */
export const beforeSetup = async (tokenAmount: number) => {
    let names: { [address: string]: string } = {};
    const [Deployer, Alice, Bob] = (await ethers.getSigners()).map(
        (x) => x.address
    );
    const userAddresses = [Deployer, Alice, Bob];
    names[Deployer] = "Deployer";
    names[Alice] = "Alice";
    names[Bob] = "Bob";
    const sf: Framework = new SuperfluidSDK.Framework({
        web3: (global as any).web3,
        version: "test",
        tokens: ["fDAI"],
		resolverAddress: RESOLVER_ADDRESS
    });

    console.log("\n");
    await sf.initialize();

    // types not properly handling this case
    const dai = await (sf.contracts as any).TestToken.at(
        sf.tokens.fDAI.address
    );
    const daix = sf.tokens.fDAIx;

    console.log(
        "Mint fDAI, approve fDAIx allowance and upgrade fDAI to fDAIx for users..."
    );
    const amount = tokenAmount.toFixed(0);
    for (let i = 0; i < userAddresses.length; i++) {
        const address = userAddresses[i];
        await dai.mint(address, ethers.utils.parseUnits(amount).toString(), {
            from: userAddresses[0],
        });
        await dai.approve(
            daix.address,
            ethers.utils.parseUnits(amount).toString(),
            { from: address }
        );
        await daix.upgrade(ethers.utils.parseUnits(amount).toString(), {
            from: address,
        });
    }
    console.log(
        "\n************** Superfluid Framework Setup Complete **************\n"
    );

    return [names, userAddresses, sf, dai, daix];
};
