import SuperfluidSDK, {
    ChainId,
    NetworkName,
} from "@superfluid-finance/sdk-core";
import { IStreamRequestFilter } from "@superfluid-finance/sdk-core/dist/main/interfaces";

const sf = new SuperfluidSDK.Framework({
    // chainId: ChainId.MATIC,
    networkName: NetworkName.MATIC,
});

(async () => {
    // sf.query query
    const test = await sf.query.listAllSuperTokens();
    console.log(test);

    // const anothaOne = await sf.query.listUserInteractedSuperTokens(
    //     "0x0037636b7f8cd7104c54d902a7009fa02a04f38b"
    // );
    // console.log(anothaOne.response);
    const filter = {
        token: "0x1305f6b6df9dc47159d12eb7ac2804d4a33173c2",
    };
    const streams = await sf.query.listStreams(filter, { first: 50, skip: 0 });
    console.log(streams.first);
    console.log(streams.skip);
    console.log(streams);
})();

console.log(sf.options.chainId);
console.log(sf.options.networkName);
console.log(sf.options.customSubgraphQueriesEndpoint);
