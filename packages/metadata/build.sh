#!/bin/bash
# This script builds the files in the main and module directories.

rm main/networks/list.cjs
rm module/networks/list.js

touch main/networks/list.cjs
touch module/networks/list.js

echo '/* eslint-disable */  
module.exports =' > main/networks/list.cjs
echo '/* eslint-disable */  
export default' > module/networks/list.js

cat networks.json | tee -a main/networks/list.cjs  module/networks/list.js > /dev/null