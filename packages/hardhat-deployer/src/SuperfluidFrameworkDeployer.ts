import ERC20PresetMinterPauser from "@superfluid-finance/ethereum-contracts/build/contracts/ERC20PresetMinterPauser.json";
import Resolver from "@superfluid-finance/ethereum-contracts/build/contracts/Resolver.json";
import SlotsBitmapLibrary from "@superfluid-finance/ethereum-contracts/build/contracts/SlotsBitmapLibrary.json";
import SuperToken from "@superfluid-finance/ethereum-contracts/build/contracts/SuperToken.json";
import SuperfluidFrameworkDeployerContract from "@superfluid-finance/ethereum-contracts/build/contracts/SuperfluidFrameworkDeployer.json";
import * as ethers from "ethers";

import "hardhat/types";
import {
    ERC1820_ADDRESS,
    ERC1820_DEPLOYER,
    ERC1820_PAYLOAD,
} from "./ERC1820Constants";

const overrides = { gasLimit: 6000000 };

const slotsBitmapLibraryPlaceholder =
    "__SlotsBitmapLibrary____________________";

const noProviderError = "No provider is defined in the `Signer` object.";
const notDeployedError = `Super Token Factory is Zero Address.
\`superfluidFrameworkDeployer.deploy(deployerSigner)\` must be called first.`;

export interface DeployMockSuperTokenResult {
    underlyingToken: ethers.Contract;
    superToken: ethers.Contract;
}

export class SuperfluidFrameworkDeployer {
    address: string;
    resolverAddress: string;

    constructor() {
        this.address = ethers.constants.AddressZero;
        this.resolverAddress = ethers.constants.AddressZero;
    }

    async deploy(deployer: ethers.Signer): Promise<string> {
        if (!deployer.provider) throw new Error(noProviderError);

        // Deploy the `ERC1820Registry` to its appropriate address (if not deployed)
        if ((await deployer.provider.getCode(ERC1820_ADDRESS)) === "0x") {
            // Give balance to ERC1820 Deployer (NOT the same as the `deployer` argument).
            // https://eips.ethereum.org/EIPS/eip-1820
            await deployer.sendTransaction({
                to: ERC1820_DEPLOYER,
                value: ethers.utils.parseEther("1"),
            });

            // Deploy raw transaction
            await deployer.provider!.sendTransaction(ERC1820_PAYLOAD);
        }

        // Deploy `Resolver`
        const resolver = await new ethers.ContractFactory(
            Resolver.abi,
            Resolver.bytecode,
            deployer
        ).deploy(overrides);

        const slotsBitmapLibrary = await new ethers.ContractFactory(
            SlotsBitmapLibrary.abi,
            SlotsBitmapLibrary.bytecode,
            deployer
        ).deploy(overrides);

        const addr = slotsBitmapLibrary.address.replace("0x", "");

        // Use `SuperfluidFrameworkDeployer`
        const superfluidFrameworkDeployer = await new ethers.ContractFactory(
            SuperfluidFrameworkDeployerContract.abi,
            SuperfluidFrameworkDeployerContract.bytecode
                .split(slotsBitmapLibraryPlaceholder)
                .join(addr),
            deployer
        ).deploy({ gasLimit: 100000000 });

        const framework = await superfluidFrameworkDeployer.getFramework();

        // Register `TestGovernance` with `Resolver`
        await resolver.set(
            "TestGovernance.test",
            framework.governance,
            overrides
        );

        // // Register `Superfluid` with `Resolver`
        await resolver.set("Superfluid.test", framework.host, overrides);

        // Update Internal State
        this.address = superfluidFrameworkDeployer.address;
        this.resolverAddress = resolver.address;

        // Return `Resolver` address
        return resolver.address;
    }

    async deployWrapperSuperToken(
        name: string,
        symbol: string,
        deployer: ethers.Signer
    ): Promise<DeployMockSuperTokenResult> {
        if (this.address === ethers.constants.AddressZero) {
            throw new Error(notDeployedError);
        }

        const framework = await new ethers.Contract(
            this.address,
            SuperfluidFrameworkDeployerContract.abi,
            deployer
        ).getFramework();

        const superTokenFactoryAddress = framework.superTokenFactory;

        const underlyingToken = await new ethers.ContractFactory(
            ERC20PresetMinterPauser.abi,
            ERC20PresetMinterPauser.bytecode,
            deployer
        ).deploy(name, symbol, overrides);

        const superTokenFactory = new ethers.Contract(
            superTokenFactoryAddress,
            ["function createERC20Wrapper(address,uint8,uint8,string,string)"],
            deployer
        );

        const tx = await superTokenFactory.createERC20Wrapper(
            underlyingToken.address,
            18, // decimals
            0, // upgradeability
            `Super ${name}`,
            `${symbol}x`,
            overrides
        );

        const { logs } = await tx.wait();

        const superTokenAddress = this.extractSuperTokenAddress(logs);

        const superToken = new ethers.Contract(
            superTokenAddress,
            SuperToken.abi,
            deployer
        );

        return { underlyingToken, superToken };
    }

    private extractSuperTokenAddress(logs: any) {
        // Event hash
        const SuperTokenCreatedEvent = ethers.utils.keccak256(
            ethers.utils.toUtf8Bytes("SuperTokenCreated(address)")
        );

        // Each 'log' in the array includes an array of 'topic's. One of those logs contains a
        // topics array that contains the `SuperTokenCreatedEvent` hash. We first find the log that
        // contains the right topics array, then we select the second element of that topics array.
        // That is the super token address.
        const paddedSuperTokenAddress = logs.find((log: any) =>
            log.topics.includes(SuperTokenCreatedEvent)
        ).topics[1];

        // The address is padded to 32 bytes, so we decode it as an address with the AbiCoder, which
        // returns an array of 1 element.
        return new ethers.utils.AbiCoder().decode(
            ["address"],
            paddedSuperTokenAddress
        )[0];
    }
}
