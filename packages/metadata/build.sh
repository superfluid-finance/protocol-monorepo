#!/usr/bin/env bash
# This script builds the files in the main and module directories.

rm main/networks/list.cjs
rm module/networks/list.js

touch main/networks/list.cjs
touch module/networks/list.js

cat > main/networks/list.cjs <<EOF
/* eslint-disable */
module.exports =
EOF
cat > module/networks/list.js <<EOF
/* eslint-disable */
export default
EOF

cat networks.json | tee -a main/networks/list.cjs  module/networks/list.js > /dev/null
