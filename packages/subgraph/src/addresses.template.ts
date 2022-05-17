import { Address } from "@graphprotocol/graph-ts";
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
    return Address.fromString(address);
}

export function getResolverAddress(): Address {
    let network = "{{network}}";
    let address = "";
    if (network == "mainnet") {
        address = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";
    }
    if (network == "goerli") {
        address = "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E";
    }
    if (network == "kovan") {
        address = "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B";
    }
    if (network == "matic") {
        address = "0xE0cc76334405EE8b39213E620587d815967af39C";
    }
    if (network == "mumbai") {
        address = "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3";
    }
    if (network == "rinkeby") {
        address = "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A";
    }
    if (network == "ropsten") {
        address = "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a";
    }
    if (network == "xdai") {
        address = "0xD2009765189164b495c110D61e4D301729079911";
    }
    if (network == "fuji") {
        address = "0x141920741bC45b962B59c833cd849bA617F7ef38";
    }
    if (network == "arbitrum-rinkeby") {
        address = "0xa2C0C70A1E922f5f060ec20EE3aF002C163b4567";
    }
    if (network == "optimism-kovan") {
        address = "0x218B65780615Ff134f9Ad810CB98839534D3C0D6";
    }
    if (network == "avalanche") {
        address = "0x24a3F04F70B7f07B9673EadD3e146391BcfEa5c1";
    }
    if (network == "arbitrum-one") {
        address = "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F";
    }
    if (network == "optimism") {
        address = "0x743B5f46BC86caF41bE4956d9275721E0531B186";
    }
    return Address.fromString(address);
}
