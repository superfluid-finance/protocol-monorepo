import { ethers } from "hardhat";
import metadata from "@superfluid-finance/metadata";

async function main() {
    const networkId = (await ethers.provider.getNetwork()).chainId;
    const RESOLVER_ADDRESS = metadata.getNetworkByChainId(networkId)?.contractsV1.resolver;
    const resolver = await ethers.getContractAt("Resolver", RESOLVER_ADDRESS || "");
    const hostAddress = await resolver.get("Superfluid.v1");
    const hostContract = await ethers.getContractAt("Superfluid", hostAddress);
    const superTokenFactoryAddress = await hostContract.getSuperTokenFactory();
    const superTokenFactoryContract = await ethers.getContractAt("SuperTokenFactory", superTokenFactoryAddress);
    console.log("superTokenFactoryAddress", superTokenFactoryAddress);
    
    const constantOutflowNFTCanonicalLogic = await superTokenFactoryContract.CONSTANT_OUTFLOW_NFT_LOGIC();
    console.log("constantOutflowNFTCanonicalLogic", constantOutflowNFTCanonicalLogic);
    const constantInflowNFTCanonicalLogic = await superTokenFactoryContract.CONSTANT_INFLOW_NFT_LOGIC();
    console.log("constantInflowNFTCanonicalLogic", constantInflowNFTCanonicalLogic);

    const superTokenFactoryLogicAddress = await hostContract.getSuperTokenFactoryLogic();
    console.log("superTokenFactoryLogicAddress", superTokenFactoryLogicAddress);
    const superTokenLogicAddress = await superTokenFactoryContract.getSuperTokenLogic();
    const superTokenLogicContract = await ethers.getContractAt("SuperToken", superTokenLogicAddress);

    const constantOutflowNFProxy = await superTokenLogicContract.CONSTANT_OUTFLOW_NFT();
    const cofNFTContract = await ethers.getContractAt("ConstantOutflowNFT", constantOutflowNFProxy);
    console.log("constantOutflowNFProxy", constantOutflowNFProxy);
    const outflowProxyLogic = await cofNFTContract.getCodeAddress();
    console.log("outflowProxyLogic", outflowProxyLogic);
    console.log("cof baseURI", await cofNFTContract.baseURI());

    const constantInflowNFProxy = await superTokenLogicContract.CONSTANT_INFLOW_NFT();
    console.log("constantInflowNFProxy", constantInflowNFProxy);
    const cifNFTContract = await ethers.getContractAt("ConstantInflowNFT", constantInflowNFProxy);
    const inflowProxyLogic = await cifNFTContract.getCodeAddress();
    console.log("inflowProxyLogic", inflowProxyLogic);
    const differentImplementations = await cofNFTContract.proxiableUUID() !== await cifNFTContract.proxiableUUID();
    console.log("nft's have different implementations:", differentImplementations);
    console.log("cif baseURI", await cofNFTContract.baseURI());

    if (!differentImplementations) throw new Error("nft's have the same implementation");

    console.log("outflow proxy logic equal canonical logic", outflowProxyLogic === constantOutflowNFTCanonicalLogic);
    console.log("inflow proxy logic equal canonical logic", inflowProxyLogic === constantInflowNFTCanonicalLogic);

    if (outflowProxyLogic !== constantOutflowNFTCanonicalLogic) throw new Error("outflow proxy logic not equal canonical logic");
    if (inflowProxyLogic !== constantInflowNFTCanonicalLogic) throw new Error("inflow proxy logic not equal canonical logic");
}

main();