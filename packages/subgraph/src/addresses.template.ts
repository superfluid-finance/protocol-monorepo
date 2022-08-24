import { Address, TypedMap } from "@graphprotocol/graph-ts";
// This file is a template file which is used for getting the address
// based on the network we set in the set-network package.json file.
// We add a bit of complexity to the package.json, but remove much
// more as a result.

export function getHostAddress(): Address {
    let network = "{{network}}";
    let address = "";
    if (network == "mainnet") {
        address = "0xa513E6E4b8f2a923D98304ec87F64353C4D5C853";
    }
    if (network == "goerli") {
        address = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
    }
    if (network == "kovan") {
        address = "0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3";
    }
    if (network == "matic") {
        address = "0x3E14dC1b13c488a8d5D310918780c983bD5982E7";
    }
    if (network == "mumbai") {
        address = "0xEB796bdb90fFA0f28255275e16936D25d3418603";
    }
    if (network == "rinkeby") {
        address = "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6";
    }
    if (network == "ropsten") {
        address = "0xF2B4E81ba39F5215Db2e05B2F66f482BB8e87FD2";
    }
    if (network == "xdai") {
        address = "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7";
    }
    if (network == "fuji") {
        address = "0xf04F2C525819691ed9ABD3D2B7109E1633795e68";
    }
    if (network == "arbitrum-rinkeby") {
        address = "0xE01F8743677Da897F4e7De9073b57Bf034FC2433";
    }
    if (network == "optimism-kovan") {
        address = "0x74b57883f8ce9F2BD330286E884CfD8BB24AC4ED";
    }
    if (network == "avalanche") {
        address = "0x60377C7016E4cdB03C87EF474896C11cB560752C";
    }
    if (network == "arbitrum-one") {
        address = "0xCf8Acb4eF033efF16E8080aed4c7D5B9285D2192";
    }
    if (network == "optimism") {
        address = "0x567c4B141ED61923967cA25Ef4906C8781069a10";
    }
    if (network == "bsc") {
        address = "0xd1e2cFb6441680002Eb7A44223160aB9B67d7E6E";
    }
    return Address.fromString(address);
}

export function getResolverAddress(): Address {
    const network = "{{network}}";
    const addresses: TypedMap<string, string> = new TypedMap<string, string>();
    addresses.set("mainnet", "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512");
    addresses.set("goerli", "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E");
    addresses.set("matic", "0xE0cc76334405EE8b39213E620587d815967af39C");
    addresses.set("mumbai", "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3");
    addresses.set("xdai", "0xD2009765189164b495c110D61e4D301729079911");
    addresses.set("fuji", "0x141920741bC45b962B59c833cd849bA617F7ef38");
    addresses.set("arbitrum-goerli", "0x21d4E9fbB9DB742E6ef4f29d189a7C18B0b59136");
    addresses.set("optimism-goerli", "0x21d4E9fbB9DB742E6ef4f29d189a7C18B0b59136");
    addresses.set("avalanche", "0x24a3F04F70B7f07B9673EadD3e146391BcfEa5c1");
    addresses.set("arbitrum-one", "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F");
    addresses.set("optimism", "0x743B5f46BC86caF41bE4956d9275721E0531B186");
    addresses.set("bsc", "0x69604aA4e9e8BF44A73C680997205Edb03A92E41");
    return Address.fromString(addresses.mustGet(network));
}

export function getNativeAssetSuperTokenAddress(): Address {
    const network = "{{network}}";
    const addresses: TypedMap<string, string> = new TypedMap<string, string>();
    addresses.set("mainnet", "0x4a679253410272dd5232b3ff7cf5dbb88f295319");
    addresses.set("goerli", "0x5943f705abb6834cad767e6e4bb258bc48d9c947");
    addresses.set("matic", "0x3ad736904e9e65189c3000c7dd2c8ac8bb7cd4e3");
    addresses.set("mumbai", "0x96b82b65acf7072efeb00502f45757f254c2a0d4");
    addresses.set("xdai", "0x59988e47a3503aafaa0368b9def095c818fdca01");
    addresses.set("fuji", "0x5735c32c38f5af0fb04a7c77c832ba4d7abffec8");
    addresses.set("arbitrum-goerli", "0xE01F8743677Da897F4e7De9073b57Bf034FC2433");
    addresses.set("optimism-goerli", "0xE01F8743677Da897F4e7De9073b57Bf034FC2433");
    addresses.set("avalanche", "0xbe916845d8678b5d2f7ad79525a62d7c08abba7e");
    addresses.set("arbitrum-one", "0xe6c8d111337d0052b9d88bf5d7d55b7f8385acd3");
    addresses.set("optimism", "0x4ac8bd1bdae47beef2d1c6aa62229509b962aa0d");
    addresses.set("bsc", "0x529a4116f160c833c61311569d6b33dff41fd657");
    return Address.fromString(addresses.mustGet(network));
}
