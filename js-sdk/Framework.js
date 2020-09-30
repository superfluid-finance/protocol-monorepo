const Web3 = require("web3");
const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("../build/abi");
const getConfig = require("./getConfig");

function Framework({
    web3Provider,
    isTruffle,
    version,
    chainId
}) {
    const contractNames = Object.keys(SuperfluidABI);

    this.chainId = chainId;
    this.version = version || "test";

    // load contracts
    this.contracts = {};
    if (!isTruffle) {
        if (!web3Provider) throw new Error("web3Provider is required");
        // load contracts from ABI
        contractNames.forEach(i => {
            const c = this.contracts[i] = TruffleContract({
                contractName: i,
                abi: SuperfluidABI[i]
            });
            c.setProvider(web3Provider);
        });
        this.web3 = new Web3(web3Provider);
    } else {
        // load contracts from truffle artifacts
        contractNames.forEach(i => {
            this.contracts[i] = global.artifacts.require(i);
        });
        // assuming web3 is available when truffle artifacts available
        this.web3 = global.web3;
    }
}

Framework.prototype.initialize = async function () {
    const chainId = this.chainId || await this.web3.eth.net.getId(); // TODO use eth.getChainId;
    console.log("chainId", chainId);

    const config = getConfig(chainId);

    console.debug("Resolver at", config.resolverAddress);
    this.resolver = await this.contracts.IResolver.at(config.resolverAddress);

    console.debug("Resolving contracts with version", this.version);
    const superfluidAddress = await this.resolver.get.call(`Superfluid.${this.version}`);
    const cfaAddress = await this.resolver.get.call(`ConstantFlowAgreementV1.${this.version}`);
    const idaAddress = await this.resolver.get.call(`InstantDistributionAgreementV1.${this.version}`);
    console.debug("Superfluid", superfluidAddress);
    console.debug("ConstantFlowAgreementV1", cfaAddress);
    console.debug("InstantDistributionAgreementV1", idaAddress);

    this.host = await this.contracts.ISuperfluid.at(superfluidAddress);
    this.agreements = {
        cfa : await this.contracts.IConstantFlowAgreementV1.at(cfaAddress),
        ida : await this.contracts.IInstantDistributionAgreementV1.at(idaAddress),
    };
};

Framework.prototype.getERC20Wrapper = async function(tokenInfo) {
    const tokenInfoSymbol = await tokenInfo.symbol.call();
    return await this.host.getERC20Wrapper.call(
        tokenInfo.address,
        `${tokenInfoSymbol}x`,
    );
};

Framework.prototype.createERC20Wrapper = async function(tokenInfo) {
    const tokenInfoName = await tokenInfo.name.call();
    const tokenInfoSymbol = await tokenInfo.symbol.call();
    const tokenInfoDecimals = await tokenInfo.decimals.call();
    await this.host.createERC20Wrapper(
        tokenInfo.address,
        tokenInfoDecimals,
        `Super ${tokenInfoName}`,
        `${tokenInfoSymbol}x`,
    );
};

module.exports = Framework;
