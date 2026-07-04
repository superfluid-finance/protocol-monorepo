/**
 * Proposes a single batched Safe transaction on Ethereum mainnet for dao.superfluid.eth:
 *   1) addOwnerWithThreshold(newOwner, currentThreshold) — threshold unchanged
 *   2) transfer full USDC balance (ERC-20) to lp.dao.superfluid.eth
 *   3) transfer full WETH balance (canonical WETH9) to lp.dao.superfluid.eth
 *
 * Uses the same Safe Transaction Service flow as gov-safe-add-owner.ts (SAFE_PROPOSER_PK,
 * optional SAFE_API_KEY / SAFE_TX_SERVICE_URL, loadOpsEnv).
 *
 * Env:
 *   DAO_SAFE              — Safe (default: dao.superfluid.eth multisig you linked)
 *   NEW_SAFE_OWNER        — owner to add (default: address below)
 *   LP_RECIPIENT_ENS      — ENS for token recipient (default: lp.dao.superfluid.eth)
 *   LP_RECIPIENT          — optional checksummed 0x…; if set, skips ENS resolution
 *   MAINNET_RPC_URL       — optional; else PROVIDER_URL_OVERRIDE, else eth-mainnet RPC from
 *                           metadata. Note: some public RPCs (e.g. cloudflare-eth.com) break
 *                           ENS; use https://ethereum.publicnode.com if resolveName fails.
 *   USDC_ADDRESS          — default USDC on Ethereum
 *   WETH_ADDRESS          — default WETH9 on Ethereum
 *   SIMULATE=1            — print batch only; no Safe API proposal
 *   SAFE_ORIGIN           — label in Safe UI (default: dao-eth-batch-owner-usdc-weth)
 *
 * Run from packages/ethereum-contracts:
 *   SIMULATE=1 npx ts-node new-ops-scripts/dao-ethereum-safe-batch-add-owner-transfer-usdc-weth.ts
 *   npx ts-node new-ops-scripts/dao-ethereum-safe-batch-add-owner-transfer-usdc-weth.ts
 */
import { ethers } from "ethers";
import type { MetaTransactionData } from "@safe-global/types-kit";

import {
  findMetadataNetwork,
  loadOpsEnv,
  readMetadataNetworks,
  resolveMetadataRpcUrl
} from "./lib/ops-env";
import { proposeSafeTx } from "./safe-ops";

const CHAIN_NAME = "eth-mainnet";
const CHAIN_ID = 1n;

/** https://app.safe.global/transactions/history?safe=eth:0xac808840f02c47C05507f48165d2222FF28EF4e1 */
const DEFAULT_DAO_SAFE = "0xac808840f02c47C05507f48165d2222FF28EF4e1";
const DEFAULT_NEW_OWNER = "0xF3B09d48BcfA57e4873c249bFe80629a49fA74E7";
const DEFAULT_LP_ENS = "lp.dao.superfluid.eth";

/** Circle USDC on Ethereum */
const DEFAULT_USDC = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48";
/** Canonical WETH9 */
const DEFAULT_WETH = "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2";

const SAFE_READ_ABI = [
  "function VERSION() view returns (string)",
  "function isOwner(address) view returns (bool)",
  "function getThreshold() view returns (uint256)"
];
const safeWriteIface = new ethers.utils.Interface([
  ...SAFE_READ_ABI,
  "function addOwnerWithThreshold(address owner, uint256 _threshold)"
]);
const erc20Iface = new ethers.utils.Interface([
  "function balanceOf(address) view returns (uint256)",
  "function transfer(address to, uint256 amount) returns (bool)"
]);

const ENS_FALLBACK_RPC = "https://ethereum.publicnode.com";

function mainnetRpcUrl(): string {
  const explicit = process.env.MAINNET_RPC_URL?.trim();
  if (explicit) return explicit;
  if (process.env.PROVIDER_URL_OVERRIDE?.trim()) return process.env.PROVIDER_URL_OVERRIDE.trim();
  const row = findMetadataNetwork(CHAIN_NAME, readMetadataNetworks());
  const u = row ? resolveMetadataRpcUrl(CHAIN_NAME, row) : null;
  if (!u) {
    throw new Error(
      "No RPC: set MAINNET_RPC_URL or PROVIDER_URL_OVERRIDE, or ensure eth-mainnet exists in metadata with publicRPCs"
    );
  }
  return u;
}

async function resolveLpRecipient(
  primary: ethers.providers.JsonRpcProvider,
  lpEns: string
): Promise<string> {
  const r = await primary.resolveName(lpEns);
  if (r) return ethers.utils.getAddress(r);
  const fallback = new ethers.providers.JsonRpcProvider(ENS_FALLBACK_RPC);
  const r2 = await fallback.resolveName(lpEns);
  if (r2) return ethers.utils.getAddress(r2);
  throw new Error(
    `Could not resolve ENS "${lpEns}". Set LP_RECIPIENT to the checksummed address or MAINNET_RPC_URL to an RPC with ENS (e.g. ${ENS_FALLBACK_RPC}).`
  );
}

async function main(): Promise<void> {
  loadOpsEnv();

  const dryRun = process.env.SIMULATE === "1";
  const daoSafe = ethers.utils.getAddress(process.env.DAO_SAFE ?? DEFAULT_DAO_SAFE);
  const newOwner = ethers.utils.getAddress(process.env.NEW_SAFE_OWNER ?? DEFAULT_NEW_OWNER);
  const usdc = ethers.utils.getAddress(process.env.USDC_ADDRESS ?? DEFAULT_USDC);
  const weth = ethers.utils.getAddress(process.env.WETH_ADDRESS ?? DEFAULT_WETH);
  const lpEns = process.env.LP_RECIPIENT_ENS?.trim() || DEFAULT_LP_ENS;

  const rpc = mainnetRpcUrl();
  const provider = new ethers.providers.JsonRpcProvider(rpc);
  const net = await provider.getNetwork();
  if (net.chainId !== 1) {
    throw new Error(`Expected Ethereum mainnet (chainId 1), got ${net.chainId}`);
  }

  let lpRecipient: string;
  const lpHex = process.env.LP_RECIPIENT?.trim();
  if (lpHex) {
    lpRecipient = ethers.utils.getAddress(lpHex);
  } else {
    lpRecipient = await resolveLpRecipient(provider, lpEns);
  }

  const safeRO = new ethers.Contract(daoSafe, SAFE_READ_ABI, provider);
  await safeRO.VERSION();

  const alreadyOwner: boolean = await safeRO.isOwner(newOwner);
  let threshold: ethers.BigNumber | null = null;
  if (!alreadyOwner) {
    threshold = await safeRO.getThreshold();
  }

  const usdcC = new ethers.Contract(usdc, erc20Iface, provider);
  const wethC = new ethers.Contract(weth, erc20Iface, provider);
  const usdcBal: ethers.BigNumber = await usdcC.balanceOf(daoSafe);
  const wethBal: ethers.BigNumber = await wethC.balanceOf(daoSafe);

  const transactions: MetaTransactionData[] = [];

  if (!alreadyOwner) {
    if (!threshold) throw new Error("unreachable");
    const data = safeWriteIface.encodeFunctionData("addOwnerWithThreshold", [newOwner, threshold]);
    transactions.push({ to: daoSafe, data, value: "0", operation: 0 });
  } else {
    console.log(`Note: ${newOwner} is already a Safe owner; skipping addOwnerWithThreshold.`);
  }

  if (usdcBal.gt(0)) {
    const data = erc20Iface.encodeFunctionData("transfer", [lpRecipient, usdcBal]);
    transactions.push({ to: usdc, data, value: "0", operation: 0 });
  } else {
    console.log("Note: USDC balance is 0; skipping USDC transfer.");
  }

  if (wethBal.gt(0)) {
    const data = erc20Iface.encodeFunctionData("transfer", [lpRecipient, wethBal]);
    transactions.push({ to: weth, data, value: "0", operation: 0 });
  } else {
    console.log("Note: WETH balance is 0; skipping WETH transfer.");
  }

  if (transactions.length === 0) {
    throw new Error("Nothing to propose: owner already added and both token balances are zero.");
  }

  console.log("======== Ethereum DAO Safe batch ========");
  console.log(`RPC: ${rpc}`);
  console.log(`Safe: ${daoSafe}`);
  console.log(`New owner: ${newOwner}${alreadyOwner ? " (already owner)" : ""}`);
  console.log(`LP recipient: ${lpRecipient} (${lpHex ? "from LP_RECIPIENT" : `from ENS ${lpEns}`})`);
  console.log(`USDC ${usdc} balance: ${usdcBal.toString()} (${ethers.utils.formatUnits(usdcBal, 6)} USDC)`);
  console.log(`WETH ${weth} balance: ${wethBal.toString()} (${ethers.utils.formatEther(wethBal)} WETH)`);
  console.log(`Batched calls: ${transactions.length}`);
  transactions.forEach((tx, i) => {
    console.log(`  [${i}] to=${tx.to} op=${tx.operation ?? 0}`);
    console.log(`       data=${tx.data}`);
  });
  console.log("");

  if (dryRun) {
    console.log("SIMULATE=1 — not proposing to Safe Transaction Service.");
    return;
  }

  const pk = process.env.SAFE_PROPOSER_PK;
  if (!pk) throw new Error("SAFE_PROPOSER_PK is required unless SIMULATE=1");

  const origin = process.env.SAFE_ORIGIN ?? "dao-eth-batch-owner-usdc-weth";
  const result = await proposeSafeTx({
    rpcUrl: rpc,
    chainId: CHAIN_ID,
    safeAddress: daoSafe,
    proposerPrivateKey: pk,
    transactions,
    apiKey: process.env.SAFE_API_KEY,
    txServiceUrl: process.env.SAFE_TX_SERVICE_URL,
    origin,
    logger: console
  });

  console.log("");
  console.log("✓ Proposed.");
  console.log(`Safe Tx Hash: ${result.safeTxHash}`);
  console.log(`Nonce: ${result.nonce}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
