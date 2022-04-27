# include .env file and export its env vars
# (-include to ignore error if it does not exist)
-include .env

all: clean install setup-yarn update solc build 

# Install proper solc version.
solc:; nix-env -f https://github.com/dapphub/dapptools/archive/master.tar.gz -iA solc-static-versions.solc_0_8_13

# Clean the repo
clean  :; forge clean

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
	forge create StakeContract --private-key ${PRIVATE_KEY} # --rpc-url 
