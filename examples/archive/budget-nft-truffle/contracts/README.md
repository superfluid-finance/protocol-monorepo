Budget NFT
==================

Create a DAO budgeting system that is mediated by NFTs and Superfluid streams.
You can deploy the contract, then mint NFTs to working groups/individuals within your organization. Minting these NFTs will also open up a Superfluid stream to the owners of each NFT. 

1) Deploy the BudgetNFT contract, and open up a Superfluid stream into the contract.
2) Use the ```issueNFT()``` function to issue an NFT to a new address - ideally a working group or individual within your DAO. 
3) Edit the stream sent to an NFT holder by using the ```editNFT()``` function
4) Burn the NFT by using the ```burnNFT()``` function - which will delete the stream associated with that NFT
5) Owners of sub NFTs can use the ```splitStream()``` & ```mergeStreams()``` functions to manage funds within additional subgroups


## Run tests

```bash
yarn install
yarn build
yarn test
```