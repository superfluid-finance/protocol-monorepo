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

// contract addresses (polygon)
export const hostAddress = "0x3e14dc1b13c488a8d5d310918780c983bd5982e7";
export const cfaV1Address = "0x6eee6060f715257b970700bc2656de21dedf074c";
export const idaV1Address = "0xb0aabba4b2783a72c52956cdef62d438eca2d7a1";
export const superTokenFactoryAddress = "0x2c90719f25b10fc5646c82da3240c76fa5bccf34";
export const superTokenLogicAddress = "0xd15c6953c0a7fcc324e835f397496d53751441e2";
export const resolverAddress = "0xe0cc76334405ee8b39213e620587d815967af39c";
export const togaAddress = "0x6aeaee5fd4d05a741723d752d30ee4d72690a8f7";
export const maticXAddress = "0x3ad736904e9e65189c3000c7dd2c8ac8bb7cd4e3";
export const maticXSymbol = "MATICx";
export const maticXName = "Super MATIC";
export const daiXAddress = "0x1305f6b6df9dc47159d12eb7ac2804d4a33173c2";
export const daiXSymbol = "DAIx";
export const daiXName = "Super DAI (PoS)";
export const daiAddress = "0x8f3cf7ad23cd3cadbd9735aff958023239c6a063";
export const daiSymbol = "DAI";
export const daiName = "(PoS) Dai Stablecoin (DAI)";
