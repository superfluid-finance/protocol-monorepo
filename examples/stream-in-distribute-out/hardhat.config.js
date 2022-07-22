require("@nomiclabs/hardhat-waffle");
require('hardhat-erc1820');

module.exports = {
    solidity: {
        version: '0.8.14',
        settings: {
            optimizer: {
                enabled: true
            }
        }
    }
}
