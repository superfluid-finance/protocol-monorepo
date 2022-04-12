// require("@nomiclabs/hardhat-waffle");
require("@nomiclabs/hardhat-truffle5");
require('hardhat-deploy');

require("dotenv").config();

// This is a sample Hardhat task. To learn how to create your own go to
// https://hardhat.org/guides/create-task.html
task("accounts", "Prints the list of accounts", async (taskArgs, hre) => {
  const accounts = await hre.ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});

const defaultNetwork = "ganache";
// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  defaultNetwork,

  solidity: {
    version: "0.8.13",
    settings: {
      optimizer: {
        enabled: true
      }
    }
  },

  networks: {
    ganache: {
      url: "http://127.0.0.1:8545",
      chain_id: "1337",
      port: process.env.GANACHE_PORT || 8545,
      // accounts: {
      //   mnemonic: `${process.env.GOERLI_MNEMONIC}`
      // }
    },
  
    // polytest: {
    //    url: `${process.env.MUMBAI_ALCHEMY_URL}`,// using alchemy instead of moralis. add your own URL in .env
    //    gasPrice: 1000000000,
    //    accounts: [`0x${process.env.MUMBAI_DEPLOYER_PRIV_KEY}`]
    // },

    // localhost: {
    //   url: "http://localhost:8545",
    // },

    // rinkeby: {
    //   url: `${process.env.RINKEBY_ALCHEMY_URL}`,
    //   gasPrice:  1500000000,
    //   accounts: [`0x${process.env.RINKEBY_DEPLOYER_PRIVATE_KEY}`]
    //   },
    // },
  },
    namedAccounts: {
      deployer: {
        default: 0, // here this will by default take the first account as deployer
      },
  }
}


// require("dotenv").config();
// const { utils } = require("ethers");
// const fs = require("fs");
// const chalk = require("chalk");

// require("@nomiclabs/hardhat-waffle");
// require("@tenderly/hardhat-tenderly");

// require("hardhat-deploy");

// require("@eth-optimism/hardhat-ovm");
// require("@nomiclabs/hardhat-ethers");

// const { isAddress, getAddress, formatUnits, parseUnits } = utils;

// /*
//       📡 This is where you configure your deploy configuration for 🏗 scaffold-eth

//       check out `packages/scripts/deploy.js` to customize your deployment

//       out of the box it will auto deploy anything in the `contracts` folder and named *.sol
//       plus it will use *.args for constructor args
// */

// //
// // Select the network you want to deploy to here:
// //
// //using polytest aka mumbai
// const defaultNetwork = "polytest";

// const mainnetGwei = 21;

// function mnemonic() {
//   try {
//     return fs.readFileSync("./mnemonic.txt").toString().trim();
//   } catch (e) {
//     if (defaultNetwork !== "localhost") {
//       console.log(
//         "☢️ WARNING: No mnemonic file created for a deploy account. Try `yarn run generate` and then `yarn run account`."
//       );
//     }
//   }
//   return "";
// }

// module.exports = {
//   defaultNetwork,

//   // if you want to deploy to a testnet, mainnet, or xdai, you will need to configure:
//   // 1. An Infura key (or similar)
//   // 2. A private key for the deployer
//   // DON'T PUSH THESE HERE!!!
//   // An `example.env` has been provided in the Hardhat root. Copy it and rename it `.env`
//   // Follow the directions, and uncomment the network you wish to deploy to.

  
//   networks: {

//     hardhat: {
//       forking: {
//         //your rpc url here
//         url: `${process.env.MUMBAI_ALCHEMY_URL}`,
//         blockNumber: 20005290,
//         accounts: [`0x${process.env.MUMBAI_DEPLOYER_PRIV_KEY}`]
//     }
//   },

//     localhost: {
//       url: "http://localhost:8545",
//       /*      
//         notice no mnemonic here? it will just use account 0 of the hardhat node to deploy
//         (you can put in a mnemonic here to set the deployer locally)
      
//       */
//     },

//     // rinkeby: {
//     //   url: `https://rinkeby.infura.io/v3/${process.env.RINKEBY_INFURA_KEY}`,
//     //   accounts: [`${process.env.RINKEBY_DEPLOYER_PRIV_KEY}`],
//     // },
//     // kovan: {
//     //   url: `https://rinkeby.infura.io/v3/${process.env.KOVAN_INFURA_KEY}`,
//     //   accounts: [`${process.env.KOVAN_DEPLOYER_PRIV_KEY}`],
//     // },
//     // mainnet: {
//     //   url: `https://mainnet.infura.io/v3/${process.env.MAINNET_INFURA_KEY}`,
//     //   accounts: [`${process.env.MAINNET_DEPLOYER_PRIV_KEY}`],
//     // },
//     // ropsten: {
//     //   url: `https://ropsten.infura.io/v3/${process.env.ROPSTEN_INFURA_KEY}`,
//     //   accounts: [`${process.env.ROPSTEN_DEPLOYER_PRIV_KEY}`],
//     // },
//     // goerli: {
//     //   url: `https://goerli.infura.io/v3/${process.env.GOERLI_INFURA_KEY}`,
//     //   accounts: [`${process.env.GOERLI_DEPLOYER_PRIV_KEY}`],
//     // },
//     // xdai: {
//     //   url: 'https://dai.poa.network',
//     //   gasPrice: 1000000000,
//     //   accounts: [`${process.env.XDAI_DEPLOYER_PRIV_KEY}`],
//     // },

//     rinkeby: {
//       url: "https://rinkeby.infura.io/v3/460f40a260564ac4a4f4b3fffb032dad", // <---- YOUR INFURA ID! (or it won't work)
      
//        //    url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXXXXX/eth/rinkeby", // <---- YOUR MORALIS ID! (not limited to infura)
      
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     kovan: {
//       url: "https://kovan.infura.io/v3/460f40a260564ac4a4f4b3fffb032dad", // <---- YOUR INFURA ID! (or it won't work)
    
//       //    url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXXXXX/eth/kovan", // <---- YOUR MORALIS ID! (not limited to infura)
      
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     mainnet: {
//       url: "https://mainnet.infura.io/v3/460f40a260564ac4a4f4b3fffb032dad", // <---- YOUR INFURA ID! (or it won't work)
      
//       //      url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXXXXXXX/eth/mainnet", // <---- YOUR MORALIS ID! (not limited to infura)
        
//       gasPrice: mainnetGwei*1000000000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     ropsten: {
//       url: "https://ropsten.infura.io/v3/460f40a260564ac4a4f4b3fffb032dad", // <---- YOUR INFURA ID! (or it won't work)
      
//       //      url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXXXXXXX/eth/ropsten",// <---- YOUR MORALIS ID! (not limited to infura)
      
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     goerli: {
//       url: "https://goerli.infura.io/v3/460f40a260564ac4a4f4b3fffb032dad", // <---- YOUR INFURA ID! (or it won't work)
      
//       //      url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXXXXXXX/eth/goerli", // <---- YOUR MORALIS ID! (not limited to infura)
      
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     xdai: {
//       url: "https://rpc.xdaichain.com/",
//       gasPrice: 1000000000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     polygon: {
//       url: "https://speedy-nodes-nyc.moralis.io/XXXXXXXXXXXXXXXXXXXx/polygon/mainnet",// <---- YOUR MORALIS ID! (not limited to infura)
//       gasPrice: 1000000000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },     
//     polytest: {
//       url: `${process.env.MUMBAI_ALCHEMY_URL}`,// using alchemy instead of moralis. add your own URL in .env
//       gasPrice: 1000000000,
//       accounts: [`0x${process.env.MUMBAI_DEPLOYER_PRIV_KEY}`]
//       },
//     },    

//     matic: {
//       url: "https://rpc-mainnet.maticvigil.com/",
//       gasPrice: 1000000000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     rinkebyArbitrum: {
//       url: "https://rinkeby.arbitrum.io/rpc",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       companionNetworks: {
//         l1: "rinkeby",
//       },
//     },
//     localArbitrum: {
//       url: "http://localhost:8547",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       companionNetworks: {
//         l1: "localArbitrumL1",
//       },
//     },
//     localArbitrumL1: {
//       url: "http://localhost:7545",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       companionNetworks: {
//         l2: "localArbitrum",
//       },
//     },
//     kovanOptimism: {
//       url: "https://kovan.optimism.io",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       ovm: true,
//       companionNetworks: {
//         l1: "kovan",
//       },
//     },
//     localOptimism: {
//       url: "http://localhost:8545",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       ovm: true,
//       companionNetworks: {
//         l1: "localOptimismL1",
//       },
//     },
//     localOptimismL1: {
//       url: "http://localhost:9545",
//       gasPrice: 0,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//       companionNetworks: {
//         l2: "localOptimism",
//       },
//     },
//     localAvalanche: {
//       url: "http://localhost:9650/ext/bc/C/rpc",
//       gasPrice: 225000000000,
//       chainId: 43112,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     fujiAvalanche: {
//       url: "https://api.avax-test.network/ext/bc/C/rpc",
//       gasPrice: 225000000000,
//       chainId: 43113,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     mainnetAvalanche: {
//       url: "https://api.avax.network/ext/bc/C/rpc",
//       gasPrice: 225000000000,
//       chainId: 43114,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     testnetHarmony: {
//       url: "https://api.s0.b.hmny.io",
//       gasPrice: 1000000000,
//       chainId: 1666700000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//     mainnetHarmony: {
//       url: "https://api.harmony.one",
//       gasPrice: 1000000000,
//       chainId: 1666600000,
//       accounts: {
//         mnemonic: mnemonic(),
//       },
//     },
//   // },
//   solidity: {
//     compilers: [
//       {
//         version: "0.7.0",
//         settings: {
//           optimizer: {
//             enabled: true,
//             runs: 200,
//           },
//         },
//       },
//       {
//         version: "0.6.7",
//         settings: {
//           optimizer: {
//             enabled: true,
//             runs: 200,
//           },
//         },
//       },
//     ],
//   },
//   ovm: {
//     solcVersion: "0.7.6",
//   },
//   namedAccounts: {
//     deployer: {
//       default: 0, // here this will by default take the first account as deployer
//     },
//   },
// };

// const DEBUG = false;

// function debug(text) {
//   if (DEBUG) {
//     console.log(text);
//   }
// }

// task("wallet", "Create a wallet (pk) link", async (_, { ethers }) => {
//   const randomWallet = ethers.Wallet.createRandom();
//   const privateKey = randomWallet._signingKey().privateKey;
//   console.log("🔐 WALLET Generated as " + randomWallet.address + "");
//   console.log("🔗 http://localhost:3000/pk#" + privateKey);
// });

// task("fundedwallet", "Create a wallet (pk) link and fund it with deployer?")
//   .addOptionalParam(
//     "amount",
//     "Amount of ETH to send to wallet after generating"
//   )
//   .addOptionalParam("url", "URL to add pk to")
//   .setAction(async (taskArgs, { network, ethers }) => {
//     const randomWallet = ethers.Wallet.createRandom();
//     const privateKey = randomWallet._signingKey().privateKey;
//     console.log("🔐 WALLET Generated as " + randomWallet.address + "");
//     let url = taskArgs.url ? taskArgs.url : "http://localhost:3000";

//     let localDeployerMnemonic;
//     try {
//       localDeployerMnemonic = fs.readFileSync("./mnemonic.txt");
//       localDeployerMnemonic = localDeployerMnemonic.toString().trim();
//     } catch (e) {
//       /* do nothing - this file isn't always there */
//     }

//     let amount = taskArgs.amount ? taskArgs.amount : "0.01";
//     const tx = {
//       to: randomWallet.address,
//       value: ethers.utils.parseEther(amount),
//     };

//     //SEND USING LOCAL DEPLOYER MNEMONIC IF THERE IS ONE
//     // IF NOT SEND USING LOCAL HARDHAT NODE:
//     if (localDeployerMnemonic) {
//       let deployerWallet = new ethers.Wallet.fromMnemonic(
//         localDeployerMnemonic
//       );
//       deployerWallet = deployerWallet.connect(ethers.provider);
//       console.log(
//         "💵 Sending " +
//           amount +
//           " ETH to " +
//           randomWallet.address +
//           " using deployer account"
//       );
//       let sendresult = await deployerWallet.sendTransaction(tx);
//       console.log("\n" + url + "/pk#" + privateKey + "\n");
//       return;
//     } else {
//       console.log(
//         "💵 Sending " +
//           amount +
//           " ETH to " +
//           randomWallet.address +
//           " using local node"
//       );
//       console.log("\n" + url + "/pk#" + privateKey + "\n");
//       return send(ethers.provider.getSigner(), tx);
//     }
//   });

// task(
//   "generate",
//   "Create a mnemonic for builder deploys",
//   async (_, { ethers }) => {
//     const bip39 = require("bip39");
//     const hdkey = require("ethereumjs-wallet/hdkey");
//     const mnemonic = bip39.generateMnemonic();
//     if (DEBUG) console.log("mnemonic", mnemonic);
//     const seed = await bip39.mnemonicToSeed(mnemonic);
//     if (DEBUG) console.log("seed", seed);
//     const hdwallet = hdkey.fromMasterSeed(seed);
//     const wallet_hdpath = "m/44'/60'/0'/0/";
//     const account_index = 0;
//     let fullPath = wallet_hdpath + account_index;
//     if (DEBUG) console.log("fullPath", fullPath);
//     const wallet = hdwallet.derivePath(fullPath).getWallet();
//     const privateKey = "0x" + wallet._privKey.toString("hex");
//     if (DEBUG) console.log("privateKey", privateKey);
//     var EthUtil = require("ethereumjs-util");
//     const address =
//       "0x" + EthUtil.privateToAddress(wallet._privKey).toString("hex");
//     console.log(
//       "🔐 Account Generated as " +
//         address +
//         " and set as mnemonic in packages/hardhat"
//     );
//     console.log(
//       "💬 Use 'yarn run account' to get more information about the deployment account."
//     );

//     fs.writeFileSync("./" + address + ".txt", mnemonic.toString());
//     fs.writeFileSync("./mnemonic.txt", mnemonic.toString());
//   }
// );

// task(
//   "mineContractAddress",
//   "Looks for a deployer account that will give leading zeros"
// )
//   .addParam("searchFor", "String to search for")
//   .setAction(async (taskArgs, { network, ethers }) => {
//     let contract_address = "";
//     let address;

//     const bip39 = require("bip39");
//     const hdkey = require("ethereumjs-wallet/hdkey");

//     let mnemonic = "";
//     while (contract_address.indexOf(taskArgs.searchFor) != 0) {
//       mnemonic = bip39.generateMnemonic();
//       if (DEBUG) console.log("mnemonic", mnemonic);
//       const seed = await bip39.mnemonicToSeed(mnemonic);
//       if (DEBUG) console.log("seed", seed);
//       const hdwallet = hdkey.fromMasterSeed(seed);
//       const wallet_hdpath = "m/44'/60'/0'/0/";
//       const account_index = 0;
//       let fullPath = wallet_hdpath + account_index;
//       if (DEBUG) console.log("fullPath", fullPath);
//       const wallet = hdwallet.derivePath(fullPath).getWallet();
//       const privateKey = "0x" + wallet._privKey.toString("hex");
//       if (DEBUG) console.log("privateKey", privateKey);
//       var EthUtil = require("ethereumjs-util");
//       address =
//         "0x" + EthUtil.privateToAddress(wallet._privKey).toString("hex");

//       const rlp = require("rlp");
//       const keccak = require("keccak");

//       let nonce = 0x00; //The nonce must be a hex literal!
//       let sender = address;

//       let input_arr = [sender, nonce];
//       let rlp_encoded = rlp.encode(input_arr);

//       let contract_address_long = keccak("keccak256")
//         .update(rlp_encoded)
//         .digest("hex");

//       contract_address = contract_address_long.substring(24); //Trim the first 24 characters.
//     }

//     console.log(
//       "⛏  Account Mined as " +
//         address +
//         " and set as mnemonic in packages/hardhat"
//     );
//     console.log(
//       "📜 This will create the first contract: " +
//         chalk.magenta("0x" + contract_address)
//     );
//     console.log(
//       "💬 Use 'yarn run account' to get more information about the deployment account."
//     );

//     fs.writeFileSync(
//       "./" + address + "_produces" + contract_address + ".txt",
//       mnemonic.toString()
//     );
//     fs.writeFileSync("./mnemonic.txt", mnemonic.toString());
//   });

// task(
//   "account",
//   "Get balance informations for the deployment account.",
//   async (_, { ethers }) => {
//     const hdkey = require("ethereumjs-wallet/hdkey");
//     const bip39 = require("bip39");
//     let mnemonic = fs.readFileSync("./mnemonic.txt").toString().trim();
//     if (DEBUG) console.log("mnemonic", mnemonic);
//     const seed = await bip39.mnemonicToSeed(mnemonic);
//     if (DEBUG) console.log("seed", seed);
//     const hdwallet = hdkey.fromMasterSeed(seed);
//     const wallet_hdpath = "m/44'/60'/0'/0/";
//     const account_index = 0;
//     let fullPath = wallet_hdpath + account_index;
//     if (DEBUG) console.log("fullPath", fullPath);
//     const wallet = hdwallet.derivePath(fullPath).getWallet();
//     const privateKey = "0x" + wallet._privKey.toString("hex");
//     if (DEBUG) console.log("privateKey", privateKey);
//     var EthUtil = require("ethereumjs-util");
//     const address =
//       "0x" + EthUtil.privateToAddress(wallet._privKey).toString("hex");

//     var qrcode = require("qrcode-terminal");
//     qrcode.generate(address);
//     console.log("‍📬 Deployer Account is " + address);
//     for (let n in config.networks) {
//       //console.log(config.networks[n],n)
//       try {
//         let provider = new ethers.providers.JsonRpcProvider(
//           config.networks[n].url
//         );
//         let balance = await provider.getBalance(address);
//         console.log(" -- " + n + " --  -- -- 📡 ");
//         console.log("   balance: " + ethers.utils.formatEther(balance));
//         console.log(
//           "   nonce: " + (await provider.getTransactionCount(address))
//         );
//       } catch (e) {
//         if (DEBUG) {
//           console.log(e);
//         }
//       }
//     }
//   }
// );

// async function addr(ethers, addr) {
//   if (isAddress(addr)) {
//     return getAddress(addr);
//   }
//   const accounts = await ethers.provider.listAccounts();
//   if (accounts[addr] !== undefined) {
//     return accounts[addr];
//   }
//   throw `Could not normalize address: ${addr}`;
// }

// task("accounts", "Prints the list of accounts", async (_, { ethers }) => {
//   const accounts = await ethers.provider.listAccounts();
//   accounts.forEach((account) => console.log(account));
// });

// task("blockNumber", "Prints the block number", async (_, { ethers }) => {
//   const blockNumber = await ethers.provider.getBlockNumber();
//   console.log(blockNumber);
// });

// task("balance", "Prints an account's balance")
//   .addPositionalParam("account", "The account's address")
//   .setAction(async (taskArgs, { ethers }) => {
//     const balance = await ethers.provider.getBalance(
//       await addr(ethers, taskArgs.account)
//     );
//     console.log(formatUnits(balance, "ether"), "ETH");
//   });

// function send(signer, txparams) {
//   return signer.sendTransaction(txparams, (error, transactionHash) => {
//     if (error) {
//       debug(`Error: ${error}`);
//     }
//     debug(`transactionHash: ${transactionHash}`);
//     // checkForReceipt(2, params, transactionHash, resolve)
//   });
// }

// task("send", "Send ETH")
//   .addParam("from", "From address or account index")
//   .addOptionalParam("to", "To address or account index")
//   .addOptionalParam("amount", "Amount to send in ether")
//   .addOptionalParam("data", "Data included in transaction")
//   .addOptionalParam("gasPrice", "Price you are willing to pay in gwei")
//   .addOptionalParam("gasLimit", "Limit of how much gas to spend")

//   .setAction(async (taskArgs, { network, ethers }) => {
//     const from = await addr(ethers, taskArgs.from);
//     debug(`Normalized from address: ${from}`);
//     const fromSigner = await ethers.provider.getSigner(from);

//     let to;
//     if (taskArgs.to) {
//       to = await addr(ethers, taskArgs.to);
//       debug(`Normalized to address: ${to}`);
//     }

//     const txRequest = {
//       from: await fromSigner.getAddress(),
//       to,
//       value: parseUnits(
//         taskArgs.amount ? taskArgs.amount : "0",
//         "ether"
//       ).toHexString(),
//       nonce: await fromSigner.getTransactionCount(),
//       gasPrice: parseUnits(
//         taskArgs.gasPrice ? taskArgs.gasPrice : "1.001",
//         "gwei"
//       ).toHexString(),
//       gasLimit: taskArgs.gasLimit ? taskArgs.gasLimit : 24000,
//       chainId: network.config.chainId,
//     };

//     if (taskArgs.data !== undefined) {
//       txRequest.data = taskArgs.data;
//       debug(`Adding data to payload: ${txRequest.data}`);
//     }
//     debug(txRequest.gasPrice / 1000000000 + " gwei");
//     debug(JSON.stringify(txRequest, null, 2));

//     return send(fromSigner, txRequest);
//   }); 
