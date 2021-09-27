import { ethers } from "hardhat";
import { ContractReceipt } from "ethers";
import { request, gql } from "graphql-request";
import SuperfluidSDK from "@superfluid-finance/js-sdk";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { IMeta } from "../interfaces";

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
    const [Deployer, Alice, Bob, Charlie, Dave, Ella, Frank] = (
        await ethers.getSigners()
    ).map((x) => x.address);
    const userAddresses = [Deployer, Alice, Bob, Charlie, Dave, Ella, Frank];
    names[Deployer] = "Deployer";
    names[Alice] = "Alice";
    names[Bob] = "Bob";
    const sf: Framework = new SuperfluidSDK.Framework({
        web3: (global as any).web3,
        version: "test",
        tokens: ["fDAI"],
        resolverAddress: RESOLVER_ADDRESS,
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
            {
                from: address,
            }
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

export const monthlyToSecondRate = (monthlyRate: number) => {
    const days = 30;
    const hours = days * 24;
    const minutes = hours * 60;
    const seconds = minutes * 60;
    return Math.round((monthlyRate / seconds) * 10 ** 18);
};

export const getCurrentBlockNumber = async () => {
    const query = gql`
        query {
            _meta {
                block {
                    number
                }
            }
        }
    `;
    const data = await subgraphRequest<IMeta>(query);
    if (!data) return 0;

    return data._meta.block.number;
};

export const subgraphRequest = async <T>(
    query: string,
    variables?: { [key: string]: any }
): Promise<T> => {
    try {
        const response = await request<T>(
            "http://localhost:8000/subgraphs/name/superfluid-test",
            query,
            variables
        );
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};

function asleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

export const waitUntilBlockIndexed = async (txnBlockNumber: number) => {
    let currentBlock: number;
    do {
        currentBlock = await getCurrentBlockNumber();
        await asleep(50);
    } while (txnBlockNumber > currentBlock);
};

export const getEventId = (receipt: ContractReceipt) => {
    return (
        receipt.transactionHash.toLowerCase() + "-" + receipt.transactionIndex
    );
};

export const getStreamId = (
    sender: string,
    receiver: string,
    token: string,
    revisionIndex: string
) => {
    return [
        sender.toLowerCase(),
        receiver.toLowerCase(),
        token.toLowerCase(),
        revisionIndex + ".0",
    ].join("-");
};
