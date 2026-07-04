const fs = require("fs");
const util = require("util");

const {ethers} = require("hardhat");
const getConfig = require("../ops-libs/getConfig");
const {
    builtTruffleContractLoader,
    ZERO_ADDRESS,
} = require("../ops-libs/common");

const SLOTS_BITMAP_LIBRARY_SELECTOR = "0x3fd4176a";

function parseColonArgs(argv) {
    const argIndex = argv.indexOf(":");
    return argIndex < 0 ? [] : argv.slice(argIndex + 1);
}

function contractAt(name, address, signerOrProvider) {
    const {abi} = builtTruffleContractLoader(name);
    return new ethers.Contract(address, abi, signerOrProvider);
}

async function getCodeAddress(provider, proxyAddress) {
    const proxiable = contractAt("UUPSProxiable", proxyAddress, provider);
    return proxiable.getCodeAddress();
}

async function callSlotsBitmapLibrary(provider, target) {
    const data = await provider.call({
        to: target,
        data: SLOTS_BITMAP_LIBRARY_SELECTOR,
    });
    return ethers.utils.getAddress("0x" + data.slice(-40));
}

async function loadSuperTokenEntry(resolver, version, provider, tokenKey) {
    const superTokenAddress = await resolver.get(
        `supertokens.${version}.${tokenKey}`
    );
    if (superTokenAddress === ZERO_ADDRESS) {
        return null;
    }
    const superToken = contractAt("SuperToken", superTokenAddress, provider);
    const underlyingAddress = await superToken.getUnderlyingToken();
    const underlying = contractAt(
        "IERC20Metadata",
        underlyingAddress,
        provider
    );
    return {
        superTokenAddress,
        underlyingAddress,
        underlyingSymbol: await underlying.symbol(),
    };
}

async function main() {
    const args = parseColonArgs(process.argv);
    if (args.length !== 1) {
        throw new Error("Wrong number of arguments");
    }
    const outputFilename = args[0];
    const skipTokens =
        process.env.SKIP_TOKENS === "1" || process.env.SKIP_TOKENS === "true";
    const protocolReleaseVersion = process.env.RELEASE_VERSION || "test";

    const provider = ethers.provider;
    const network = await provider.getNetwork();
    const chainId = network.chainId;
    const config = getConfig(chainId);

    console.log("network ID:", network.chainId);
    console.log("chain ID:", chainId);
    console.log("protocol release version:", protocolReleaseVersion);

    let output = "";
    if (config.isTestnet) {
        output += "IS_TESTNET=1\n";
    }

    const resolverAddress = config.resolverAddress;
    if (!resolverAddress) {
        throw new Error(
            `No resolver address configured for chainId ${chainId}`
        );
    }

    const resolver = contractAt("IResolver", resolverAddress, provider);
    const loaderAddress = await resolver.get("SuperfluidLoader-v1");
    const loader = contractAt("SuperfluidLoader", loaderAddress, provider);
    const framework = await loader.loadFramework(protocolReleaseVersion);

    const hostAddress = framework.superfluid;
    const host = contractAt("ISuperfluid", hostAddress, provider);
    const cfaAddress = framework.agreementCFAv1;
    const idaAddress = framework.agreementIDAv1;
    const gdaAddress = framework.agreementGDAv1;

    output += `NETWORK_ID=${chainId}\n`;
    output += `RESOLVER=${resolverAddress}\n`;
    output += `SUPERFLUID_LOADER=${loaderAddress}\n`;
    output += `SUPERFLUID_HOST_PROXY=${hostAddress}\n`;
    output += `SUPERFLUID_HOST_LOGIC=${await getCodeAddress(provider, hostAddress)}\n`;
    output += `SUPERFLUID_GOVERNANCE=${await host.getGovernance()}\n`;

    try {
        const govAddress = await host.getGovernance();
        output += `SUPERFLUID_GOVERNANCE_LOGIC=${await getCodeAddress(
            provider,
            govAddress
        )}\n`;
    } catch (e) {
        // governance may not be a UUPS proxy on all networks
    }

    output += `SUPER_TOKEN_FACTORY_PROXY=${await host.getSuperTokenFactory()}\n`;
    output += `SUPER_TOKEN_FACTORY_LOGIC=${await host.getSuperTokenFactoryLogic()}\n`;
    output += `CFA_PROXY=${cfaAddress}\n`;
    output += `CFA_LOGIC=${await getCodeAddress(provider, cfaAddress)}\n`;
    output += `IDA_PROXY=${idaAddress}\n`;
    output += `SLOTS_BITMAP_LIBRARY=${await callSlotsBitmapLibrary(
        provider,
        idaAddress
    )}\n`;
    output += `IDA_LOGIC=${await getCodeAddress(provider, idaAddress)}\n`;
    output += `GDA_PROXY=${gdaAddress}\n`;
    output += `GDA_SLOTS_BITMAP_LIBRARY=${await callSlotsBitmapLibrary(
        provider,
        gdaAddress
    )}\n`;
    output += `GDA_LOGIC=${await getCodeAddress(provider, gdaAddress)}\n`;

    const gda = contractAt(
        "GeneralDistributionAgreementV1",
        gdaAddress,
        provider
    );
    const poolBeaconAddress = await gda.superfluidPoolBeacon();
    const poolBeacon = contractAt(
        "SuperfluidUpgradeableBeacon",
        poolBeaconAddress,
        provider
    );
    output += `SUPERFLUID_POOL_DEPLOYER_LIBRARY=${await gda.SUPERFLUID_POOL_DEPLOYER_ADDRESS()}\n`;
    output += `SUPERFLUID_POOL_BEACON=${poolBeaconAddress}\n`;
    output += `SUPERFLUID_POOL_LOGIC=${await poolBeacon.implementation()}\n`;

    const superTokenFactoryAddress = await host.getSuperTokenFactory();
    const superTokenFactory = contractAt(
        "ISuperTokenFactory",
        superTokenFactoryAddress,
        provider
    );
    const superTokenLogicAddress = await superTokenFactory.getSuperTokenLogic();
    output += `SUPER_TOKEN_LOGIC=${superTokenLogicAddress}\n`;

    const superTokenLogic = contractAt(
        "SuperToken",
        superTokenLogicAddress,
        provider
    );

    try {
        const poolAdminNFTProxyAddress = await superTokenLogic.POOL_ADMIN_NFT();
        output += `POOL_ADMIN_NFT_PROXY=${poolAdminNFTProxyAddress}\n`;
        output += `POOL_ADMIN_NFT_LOGIC=${await getCodeAddress(
            provider,
            poolAdminNFTProxyAddress
        )}\n`;

        const poolMemberNFTProxyAddress =
            await superTokenLogic.POOL_MEMBER_NFT();
        output += `POOL_MEMBER_NFT_PROXY=${poolMemberNFTProxyAddress}\n`;
        output += `POOL_MEMBER_NFT_LOGIC=${await getCodeAddress(
            provider,
            poolMemberNFTProxyAddress
        )}\n`;
    } catch (e) {
        console.warn(
            "POOL_ADMIN_NFT or POOL_MEMBER_NFT probably not deployed yet"
        );
    }

    try {
        output += `ERC2771_FORWARDER=${await host.getERC2771Forwarder()}\n`;
        output += `SIMPLE_FORWARDER=${await host.SIMPLE_FORWARDER()}\n`;
    } catch (e) {
        console.warn("[Simple|ERC2771]Forwarder probably not deployed yet");
    }

    if (!skipTokens) {
        for (const tokenName of config.tokenList) {
            const entry = await loadSuperTokenEntry(
                resolver,
                protocolReleaseVersion,
                provider,
                tokenName
            );
            if (!entry) {
                continue;
            }
            output += `SUPER_TOKEN_${tokenName.toUpperCase()}=${
                entry.superTokenAddress
            }\n`;
            output += `NON_SUPER_TOKEN_${entry.underlyingSymbol.toUpperCase()}=${
                entry.underlyingAddress
            }\n`;
        }

        if (config.nativeTokenSymbol) {
            const nativeKey = `${config.nativeTokenSymbol}x`;
            const entry = await loadSuperTokenEntry(
                resolver,
                protocolReleaseVersion,
                provider,
                nativeKey
            );
            if (entry) {
                output += `SUPER_TOKEN_NATIVE_COIN=${entry.superTokenAddress}\n`;
            }
        }
    }

    if (config.metadata?.contractsV1?.cfaV1Forwarder) {
        output += `CFAV1_FORWARDER=${config.metadata.contractsV1.cfaV1Forwarder}\n`;
    }
    if (config.metadata?.contractsV1?.gdaV1Forwarder) {
        output += `GDAV1_FORWARDER=${config.metadata.contractsV1.gdaV1Forwarder}\n`;
    }
    if (config.metadata?.contractsV1?.toga) {
        output += `TOGA=${config.metadata.contractsV1.toga}\n`;
    }
    if (config.metadata?.contractsV1?.batchLiquidator) {
        output += `BATCH_LIQUIDATOR=${config.metadata.contractsV1.batchLiquidator}\n`;
    }
    if (config.metadata?.contractsV1?.flowScheduler) {
        output += `FLOW_SCHEDULER=${config.metadata.contractsV1.flowScheduler}\n`;
    }
    if (config.metadata?.contractsV1?.vestingScheduler) {
        output += `VESTING_SCHEDULER=${config.metadata.contractsV1.vestingScheduler}\n`;
    }

    await util.promisify(fs.writeFile)(outputFilename, output);
}

module.exports = main;
