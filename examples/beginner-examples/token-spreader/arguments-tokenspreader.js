const goerliHostAddress = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
const goerliFDAIXAddress = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00"; 

module.exports = [
    goerliHostAddress,
    goerliFDAIXAddress
];

// npx hardhat verify --network goerli --constructor-args arguments-tokenspreader.js [contractaddress]