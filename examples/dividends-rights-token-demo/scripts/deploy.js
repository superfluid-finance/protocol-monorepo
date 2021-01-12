const {
    web3tx
} = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/ethereum-contracts");
const DividendRightsToken = artifacts.require("DividendRightsToken");

module.exports = async function (callback, argv) {
    const errorHandler = err => { if (err) throw err; };

    try {
        global.web3 = web3;

        const version = process.env.RELEASE_VERSION || "test";
        console.log("release version:", version);

        const sf = new SuperfluidSDK.Framework({
            chainId: 5,
            version: version,
            web3Provider: web3.currentProvider
        });
        await sf.initialize();

        const daiAddress = await sf.resolver.get("tokens.fDAI");
        const dai = await sf.contracts.TestToken.at(daiAddress);
        const daixWrapper = await sf.getERC20Wrapper(dai);
        const daix = await sf.contracts.ISuperToken.at(daixWrapper.wrapperAddress);
    
        const app = await web3tx(DividendRightsToken.new, "Deploy DividendRightsToken")(
            "Dividend Rights Token", "DRT",
            daix.address,
            sf.host.address, sf.agreements.ida.address,
        );
        console.log("App deployed at", app.address);
        callback();
    } catch (err) {
        callback(err);
    }
}

