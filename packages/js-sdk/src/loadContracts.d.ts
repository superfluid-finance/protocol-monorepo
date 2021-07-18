import { ethers, Contract, utils } from "ethers"; 
import Web3 from "web3";
import TruffleContract from "@truffle/contract";

interface EthersWithSigner {
    getSigner(): () => ethers.Signer
}

type AbiContainer = Pick<utils.Interface, "abi">;
type ContractLoader = (name: string) => AbiContainer;

declare function setTruffleContractDefaults(c: TruffleContract.Contract, networkId: number, from: string): void;

declare function defaultContractLoader(name: string): {contractName: string, abi: AbiContainer}

interface AdaptedContractOpts {
    address: string;
    abi: AbiContainer;
    ethers: EthersWithSigner;
}
declare function getAdaptedContract({address, abi, ethers}: AdaptedContractOpts): Contract;

declare function loadContracts({ isTruffle, ethers, web3, from, additionalContracts, contractLoader, networkId, }: {
    isTruffle: boolean;
    ethers?: EthersWithSigner;
    web3?: Web3;
    from: string;
    additionalContracts?: string[];
    contractLoader: ContractLoader;
    networkId: number;
}): Promise<Contract[] | Web3.Eth.Contract[] | TruffleContract.Contract[]>;

export = loadContracts;