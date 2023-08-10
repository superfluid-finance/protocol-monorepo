import { Address, BigInt } from "@graphprotocol/graph-ts";

export const TRUE = "true";
export const FALSE = "false";
export const DEFAULT_DECIMALS = 18;
export const DEFAULT_REWARD_ADDRESS = Address.fromString("0x0000000000000000000000000000000000000045");
export const LIQUIDATION_PERIOD = BigInt.fromI32(14400);
export const PATRICIAN_PERIOD = BigInt.fromI32(1800);
export const FAKE_INITIAL_BALANCE = BigInt.fromI32(1000);
export const FAKE_SUPER_TOKEN_TOTAL_SUPPLY = BigInt.fromI32(1000000);

// @note these addresses are lowercased and not checksummed
// because the graph normalizes addresses to lowercase prior
// to storing
// user addresses
export const alice = "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266";
export const bob = "0x70997970c51812dc3a010c7d01b50e0d17dc79c8";
export const charlie = "0x3c44cdddb6a900fa2b585dd299e03d12fa4293bc";
export const delta = "0x90f79bf6eb2c4f870365e785982e1f101e93b906";
export const echo = "0x15d34aaf54267db7d7c367839aaf71a00a2c6a65";
export const superfluidPool = "0xd8da6bf26964af9d7eed9e03e53415d37aa96045";

// contract addresses (polygon)
export const hostAddress = "0xeb796bdb90ffa0f28255275e16936d25d3418603";
export const cfaV1Address = "0x49e565ed1bdc17f3d220f72df0857c26fa83f873";
export const idaV1Address = "0x804348d4960a61f2d5f9ce9103027a3e849e09b8";
export const superTokenFactoryAddress = "0x200657e2f123761662567a1744f9acae50df47e6";
export const superTokenLogicAddress = "0x7b043b577a10b06296fe0bd0402f5025d97a3839";
export const resolverAddress = "0x8c54c83fbde3c59e59dd6e324531fb93d4f504d3";
// this is not the actual TOGA
export const togaAddress = "0x6aeaee5fd4d05a741723d752d30ee4d72690a8f7";
export const maticXAddress = "0x96b82b65acf7072efeb00502f45757f254c2a0d4";
export const maticXSymbol = "MATICx";
export const maticXName = "Super MATIC";
export const daiXAddress = "0x5d8b4c2554aeb7e86f387b4d6c00ac33499ed01f";
export const daiXSymbol = "DAIx";
export const daiXName = "Super DAI (PoS)";
export const daiAddress = "0x15f0ca26781c3852f8166ed2ebce5d18265cceb7";
export const daiSymbol = "DAI";
export const daiName = "(PoS) Dai Stablecoin (DAI)";
