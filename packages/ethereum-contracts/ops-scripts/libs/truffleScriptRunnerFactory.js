/**
 * @dev Parse colon marked arguments
 *
 * NOTE:
 * Provide arguments to the script through ":" separator
 */
function parseColonArgs(argv) {
    const argIndex = argv.indexOf(":");
    if (argIndex < 0) {
        console.log("No colon arguments");
        return [];
    } else {
        const args = argv.slice(argIndex + 1);
        return args;
    }
}

/**
 * @dev Script runner for logic function
 *
 * This is development framework dependent, and it's currently for Truffle.
 *
 * runnerOpts.skipArgv:
 *   - most scripts supports argv followed by an options, but for compatibility issue
 *     some script wants to skip the argv. This option enables the hack.
 */
module.exports = function (ctxFn, logicFn, runnerOpts) {
    return async function (cb, argv, options = {}) {
        try {
            const {artifacts, web3, truffleDetected} = ctxFn();

            let args;
            if (runnerOpts.skipArgv) {
                // skip argv arguments
                options = argv || {};
            } else {
                // Parse colon indicated arguments
                args = parseColonArgs(argv || process.argv);
                runnerOpts.doNotPrintColonArgs ||
                    console.log("Colon arguments", args);
            }

            // if isTruffle is not set explicitly
            if (!("isTruffle" in options)) {
                if ("DISABLE_NATIVE_TRUFFLE" in process.env) {
                    options.isTruffle = !process.env.DISABLE_NATIVE_TRUFFLE;
                } else {
                    options.isTruffle = truffleDetected;
                }
            }

            // normalize web3 environment
            console.log(
                "use truffle native environment (isTruffle)",
                options.isTruffle
            );
            if (options.isTruffle) {
                if (options.web3) {
                    throw Error(
                        "Flag 'isTruffle' cannot be 'true' when using a web3 instance."
                    );
                }
                // set these globally so that it's available throughout the executions
                global.web3 = web3;
                global.artifacts = artifacts;
            } else {
                if (!truffleDetected) {
                    if (!options.web3) {
                        throw Error(
                            "A web3 instance is not provided when not using truffle."
                        );
                    }
                    global.web3 = options.web3;
                } else {
                    // use web3 of truffle
                    options.web3 = global.web3 = web3;
                }
            }

            // Use common environment variables
            options.protocolReleaseVersion =
                options.protocolReleaseVersion ||
                process.env.RELEASE_VERSION ||
                "test";
            console.log(
                "protocol release version:",
                options.protocolReleaseVersion
            );

            const retVal = await logicFn(args, options);
            cb();
            return retVal;
        } catch (err) {
            cb(err);
        }
    };
};
