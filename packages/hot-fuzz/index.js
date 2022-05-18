function hotfuzzPatchTruffleConfig(c) {
    if (process.env.HOT_FUZZ_MODE) {

        // use different build directory in order to not interfere with regular builds
        c.contracts_build_directory = "./build/hot-fuzz-contracts";

        // create settings field if not exists
        c.compilers.solc.settings = {
            ...c.compilers.solc.settings,
        };

        c.compilers.solc.settings.libraries = {
            ...c.compilers.solc.settings.libraries,
            "@superfluid-finance/ethereum-contracts/contracts/libs/SlotsBitmapLibrary.sol":
                {
                    SlotsBitmapLibrary: "0x" + "0".repeat(38) + "ff",
                },
        };

        c.compilers.solc.settings.optimizer = {
            enabled: true,
            runs: 200,
        };

    }
}

module.exports = {
    hotfuzzPatchTruffleConfig,
};
