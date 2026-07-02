#!/usr/bin/env bash
# shellcheck disable=SC2207

set -e

cd "$(dirname "$0")/.." || exit 1

CONTRACTS=( $(jq -r .[] tasks/bundled-abi-contracts-list.json) ) || exit 2

ARTIFACT_INDEX=$(mktemp)
trap 'rm -f "$ARTIFACT_INDEX"' EXIT

while IFS= read -r -d '' artifact; do
    name=$(jq -r .contractName "$artifact")
    printf '%s\t%s\n' "$name" "$artifact" >> "$ARTIFACT_INDEX"
done < <(find build/hardhat -type f -name '*.json' ! -name '*.dbg.json' -print0)

{
    echo "if (typeof module === \"undefined\") module = {};"
    echo "// eslint-disable-next-line no-unused-vars"
    echo "let Superfluid_ABI;"
    echo "Superfluid_ABI = module.exports = {"

    # parallel processing of abi inputs
    echo "${CONTRACTS[@]}" | xargs -n1 -P4 bash -c "
        artifact=\$(awk -F '\t' -v name=\"\$1\" '\$1 == name { print \$2; exit }' \"$ARTIFACT_INDEX\")
        if [[ -z \"\$artifact\" ]]; then
            echo \"Missing hardhat artifact for contract: \$1\" >&2
            exit 3
        fi
        {
            echo -n \"    \$1: \"
            jq \".abi\" \"\$artifact\" || exit 3
            echo ','
        } > build/bundled-abi.\$1.frag
        " --
    cat build/bundled-abi.*.frag
    rm build/bundled-abi.*.frag

    echo "};"

} > build/bundled-abi.js
node -e 'console.log(JSON.stringify(require("./build/bundled-abi")))' > build/bundled-abi.json

cp tasks/bundled-abi-contracts-list.json build/
