# include .env file and export its env vars
# (-include to ignore error if it does not exist)
-include .env

all: clean remove install setup-yarn update solc build 

# Install proper solc version.
solc:; nix-env -f https://github.com/dapphub/dapptools/archive/master.tar.gz -iA solc-static-versions.solc_0_8_13

# Clean the repo
clean  :; forge clean

# remove modules
remove :; rm -rf .gitmodules && rm -rf .git/modules/* && rm -rf lib && touch .gitmodules

# Install the Modules
install :; 
	forge install dapphub/ds-test 
	forge install foundry-rs/forge-std

# # Remapping
remappings :; 
	forge remappings > remappings.txt
	echo "@superfluid-finance/=node_modules/@superfluid-finance" >> remappings.txt
	echo "@openzeppelin/=node_modules/@openzeppelin" >> remappings.txt

# Update Dependencies
update:; forge update

# Builds
build  :; forge clean && forge build --optimize --optimize-runs 1000000

setup-yarn:
	yarn 

local-node: setup-yarn 
	yarn hardhat node 

deploy:
	forge create --rpc-url ${GOERLI_PROVIDER_URL} \
	--constructor-args ${END_TIME} ${CFA_ADDRESS} ${SUPERTOKEN_ADDRESS} ${SENDER_ADDRESS} ${RECEIVER_ADDRESS} \
	--private-key ${PRIVATE_KEY} src/SimpleACLCloseResolver.sol:SimpleACLCloseResolver

verify:
	forge verify-contract --chain-id ${CHAIN_ID} --num-of-optimizations 1000000 --constructor-args \
    (cast abi-encode "constructor(uint256,address,address,address,address)" ${END_TIME} ${CFA_ADDRESS} ${SUPERTOKEN_ADDRESS} ${SENDER_ADDRESS} ${RECEIVER_ADDRESS}) \
    --compiler-version v0.8.13 0x312d8670b1333d6caa30fc1f7f4174f4475cabaa src/SimpleACLCloseResolver.sol:SimpleACLCloseResolver ${ETHERSCAN_API_KEY}
