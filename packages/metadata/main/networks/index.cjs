const nl = require("./list.cjs");

module.exports = {
    networks: nl,

    mainnets: nl.filter((e) => !e.isTestnet),

    testnets: nl.filter((e) => e.isTestnet),

    getNetworkByChainId: function (chainId) {
        return this.networks.filter((n) => n.chainId === chainId)[0];
    },

    getNetworkByName: function (name) {
        return this.networks.filter((n) => n.name === name)[0];
    },

    getNetworkByShortName: function (shortName) {
        return this.networks.filter((n) => n.shortName === shortName)[0];
    },
};
