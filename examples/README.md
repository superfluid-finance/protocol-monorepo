# Welcome to the Superfluid Examples Repo 🚀

Here, you'll find examples that can help you get started with Superfluid. 

To add an example, you can open a PR and contact the Superfluid team in the #developers channel of our [Discord](http://discord.superfluid.finance).

### A Note on Testing and Best Practices
We recommend that all testing is done using the Superfluid SDK Core, as the JS-SDK is no longer being actively maintained. Our team is in the process of re-writing several examples within this repository using the SDK Core instead of the JS SDK. If you'd like to involve yourself in this process, you may do so [here](https://github.com/superfluid-finance/protocol-monorepo/issues/651)

##### Here's a brief overview of each example:

1) The Tradeable Cashflow
    - Mint an NFT that doubles as a super app which takes in cashflows and redirects them to the NFT's owner
2) The Streaming Call Option
    - Proof of concept which allows you to create a tradeable option NFT where premium is paid for over time via a stream
3) Rewards Distribution Token
    - A ERC20 token that tokenizes units in Superfluid [Instant Distribution Agreements](https://docs.superfluid.finance/superfluid/protocol-developers/interactive-tutorials/instant-distribution). 
4) Continuous Auction
    - A continuous auction where users can enter by sending a stream to the contract. In principle, the highest bid is the winner of the auction.
5) Flowlottery
    - A game of chance built on Superfluid. Users join the game by sending a stream to our contract. All incoming streams are summed and the resulting reward stream is sent to the winner, until a new one is chosen.
6) NFT Billboard
    - An iteration on the Tradeable Cashflow example that makes use of UserData to display content on a digital billboard. Made with Scaffold-Eth
7) Budget NFT
    - A DAO budgeting framework that makes use of NFTs which represent a portion of DAO cash flows. These NFTs are meant to be distributed to working groups or individuals within DAOs
8) SDK Redux UI Examples
    - Make use of our SDK Redux to build highly performant front end Dapps with Superfluid. There are many helpful UI components in these examples that make use of advanced front end technologies and our new sdks.
9) Simple ACL Close
    - An example which makes use of the new Superfluid ACL features and Gelato to automatically close streams at a future date
10) Employment Based Loan
    - A smart contract framework for allowing employees to borrow money using their salary as a form of automatic repayment.


#### What Other Examples Would You Like to See?
- Please reach out to our team with example suggestions, or feel free to write your own and open up a PR!
- We also have several bounties open regarding current examples that need to be re-written with the latest technologies. Check out our issues to learn more. 