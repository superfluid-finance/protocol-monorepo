import sfMetaData from "@superfluid-finance/metadata";

export const chainIdToData = new Map(
    sfMetaData.networks.map((x) => [
        x.chainId,
        {
            subgraphAPIEndpoint: x.subgraphV1.hostedEndpoint,
            name: x.shortName,
            addresses: {
                network: x.shortName,
                hostStartBlock: x.startBlockV1,
                hostAddress: x.contractsV1.host,
                cfaAddress: x.contractsV1.cfaV1,
                idaAddress: x.contractsV1.idaV1,
                superTokenFactoryAddress: x.contractsV1.superTokenFactory,
                resolverV1Address: x.contractsV1.resolver,
            },
        },
    ])
);
