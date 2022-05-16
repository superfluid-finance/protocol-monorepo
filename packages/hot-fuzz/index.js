function hotfuzzPatchTruffleConfig(c) {
    if (process.env.HOT_FUZZ_MODE) {
        c.compilers.solc.settings.libraries = {
            ...c.compilers.solc.settings.libraries,
            "@superfluid-finance/ethereum-contracts/contracts/libs/SlotsBitmapLibrary.sol":
                {
                    SlotsBitmapLibrary: "0x" + "0".repeat(38) + "ff",
                },
        };
    }
}

module.exports = {
    hotfuzzPatchTruffleConfig,
};
