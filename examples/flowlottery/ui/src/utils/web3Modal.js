import Web3Modal from "web3modal"; //import WalletConnectProvider from "@walletconnect/web3-provider";

// Enter a valid infura key here to avoid being rate limited
// You can get a key for free at https://infura.io/register
const INFURA_ID = "90db0e566ddf4de7b05ed1e57306bbe4";

// Web3Modal also supports many other wallets.
// You can see other options at https://github.com/Web3Modal/web3modal
export const web3Modal = new Web3Modal({
  network: "goerli",
  cacheProvider: true
  // providerOptions: {
  //   walletconnect: {
  //     package: WalletConnectProvider,
  //     options: {
  //       infuraId: INFURA_ID
  //     }
  //   }
  // }
});

export const logoutOfWeb3Modal = async function() {
  await web3Modal.clearCachedProvider();
  window.location.reload();
};
