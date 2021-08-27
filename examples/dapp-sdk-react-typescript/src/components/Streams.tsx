import { ReactElement } from "react";
import { useContext } from "react";
import { FC } from "react";
import {AccountAndNetworkScopedContext} from "./AccountScoper";
import {StreamDetails} from "./StreamDetails";

export const Streams: FC<any> = (): ReactElement => {
    const accountAndNetworkScopedContext = useContext(AccountAndNetworkScopedContext);

    const activeStreams = accountAndNetworkScopedContext.activeStreams;
    const inactiveStreams = accountAndNetworkScopedContext.inactiveStreams;

    return (
        <>
            <h3>Active streams:</h3>
            {
                !activeStreams && <p>Loading...</p>
            }
            {
                activeStreams && activeStreams.map(x => <><StreamDetails key={x.transactionHash} transactionHash={x.transactionHash} /><br/></>)
            }
            <h3>Inactive streams:</h3>
            {
                !inactiveStreams && <p>Loading...</p>
            }
            {
                inactiveStreams && inactiveStreams.map(x => <><StreamDetails key={x.transactionHash} transactionHash={x.transactionHash} /><br/></>)
            }
        </>
    );
};
