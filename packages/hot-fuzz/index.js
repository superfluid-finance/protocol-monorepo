function addr(a) { "0x" + "0".repeat(40 - a.length) + a; }

function hotfuzzPatchTruffleConfig(c) {
    if (process.env.HOT_FUZZ_MODE) {

        // use different build directory in order to not interfere with regular builds
        c.contracts_build_directory = "./build/hot-fuzz-contracts";

        // create settings field if not exists
        c.compilers.solc.settings = {
            ...c.compilers.solc.settings,
        };

        // Manual library mappings, these should match deployContracts in "echidna.yaml".
        c.compilers.solc.settings.libraries = {
            ...c.compilers.solc.settings.libraries,
            "@superfluid-finance/ethereum-contracts/contracts/libs/SlotsBitmapLibrary.sol": {
                SlotsBitmapLibrary: addr("f01"),
            },
            // to generate:
            // $ (j=0;sed -nE 's/^library\s+(\w+)\s+\{/\1/pg' contracts/utils/SuperfluidFrameworkDeploymentSteps.sol | sort | while read i;do echo "$i: addr(\"f1$(printf "%x" $j)\")";j=$((j+1));done)
            "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol": {
                CFAv1ForwarderDeployerLibrary: addr("f10"),
                IDAv1ForwarderDeployerLibrary: addr("f11"),
                ProxyDeployerLibrary: addr("f12"),
                SuperfluidCFAv1DeployerLibrary: addr("f13"),
                SuperfluidGovDeployerLibrary: addr("f14"),
                SuperfluidHostDeployerLibrary: addr("f15"),
                SuperfluidIDAv1DeployerLibrary: addr("f16"),
                SuperfluidLoaderDeployerLibrary: addr("f17"),
                SuperfluidNFTLogicDeployerLibrary: addr("f18"),
                SuperfluidPeripheryDeployerLibrary: addr("f19"),
                SuperTokenDeployerLibrary: addr("f1a"),
                TokenDeployerLibrary: addr("f1b"),
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
