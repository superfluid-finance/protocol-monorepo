const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/ethereum-contracts");
const Exchange = artifacts.require("Exchange");

module.exports = async function(callback, argv) {
  const errorHandler = err => {
    if (err) throw err;
  };

  try {
    global.web3 = web3;

    //const version = process.env.RELEASE_VERSION || "test";
    const version = "0.1.2-preview-20201014";

    console.log("release version: ", version);
    /*
    const account = await web3.eth.accounts[0];
    console.log("my account is: ", account);
    console.log("my balance is: ", await web3.eth.getBalance(account));
    */
    const sf = new SuperfluidSDK.Framework({
      chainId: 5,
      version: version,
      web3Provider: web3.currentProvider
    });
    await sf.initialize();

    daiAddress = await sf.resolver.get("tokens.fDAI");
    dai = await sf.contracts.TestToken.at(daiAddress);
    usdcAddress = await sf.resolver.get("tokens.fUSDC");
    usdc = await sf.contracts.TestToken.at(usdcAddress);

    const daixWrapper = await sf.getERC20Wrapper(dai);
    daix = await sf.contracts.ISuperToken.at(daixWrapper.wrapperAddress);
    const usdcxWrapper = await sf.getERC20Wrapper(usdc);
    usdcx = await sf.contracts.ISuperToken.at(usdcxWrapper.wrapperAddress);

    console.log("Daix Address :", daix.address);
    console.log("USDX Address :", usdcx.address);

    const app = await web3tx(Exchange.new, "Deploy Exchange")(
      sf.host.address,
      sf.agreements.cfa.address,
      usdcx.address,
      daix.address
    );
    console.log("App deployed at", app.address);
    callback();
  } catch (err) {
    callback(err);
  }
};
