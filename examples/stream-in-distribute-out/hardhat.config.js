require("@nomiclabs/hardhat-waffle");
require('hardhat-erc1820');
require('hardhat-dependency-compiler');

module.exports = {
    solidity: {
        version: '0.8.13',
        settings: {
            optimizer: {
                enabled: true
            }
        }
    },
	dependencyCompiler: {
		paths: [
		  '@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol',
		],
	  }
}
