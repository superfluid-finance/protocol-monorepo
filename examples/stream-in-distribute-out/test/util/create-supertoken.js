const ISuperTokenFactory = require("@superfluid-finance/ethereum-contracts/build/contracts/ISuperTokenFactory.json");
const ISuperToken = require("@superfluid-finance/ethereum-contracts/build/contracts/ISuperToken.json");
const ISuperfluid = require("@superfluid-finance/ethereum-contracts/build/contracts/ISuperfluid.json")



const createSuperToken = async (underlyingAddress, name, symbol, sf, deployer) => {
    const host = new ethers.Contract(sf.settings.config.hostAddress, ISuperfluid.abi);
    const superTokenFactoryAddress = await host
        .connect(deployer)
        .getSuperTokenFactory();

    const superTokenFactory = new ethers.Contract(
        superTokenFactoryAddress,
        ISuperTokenFactory.abi
    );

    const tx = await superTokenFactory
        .connect(deployer)
        .functions["createERC20Wrapper(address,uint8,string,string)"](
            underlyingAddress,
            1,
            name,
            symbol
        );

    const result = await tx.wait();

    const address = result.events.find((e) => e.event === "SuperTokenCreated").args[0];

    return new ethers.Contract(address, ISuperToken.abi, deployer);
}

module.exports = createSuperToken;
