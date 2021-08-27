import {ReactElement, useState} from "react";
import { useContext } from "react";
import { FC } from "react";
import {AccountAndNetworkScopedContext} from "./AccountScoper";
import {useDispatch} from "react-redux";
import {fetchStreamDetails} from "dapp-sdk/build/main/mainSlice";

interface Props {
    transactionHash: string;
}

export const StreamDetails: FC<Props> = ({transactionHash}): ReactElement => {
    const accountAndNetworkScopedContext = useContext(AccountAndNetworkScopedContext);
    const dispatch = useDispatch();

    const streamDetails = accountAndNetworkScopedContext.network.streamDetails[transactionHash];

    const [fetchDetails, setFetchDetails] = useState(false);
    const [hasFetched, setHasFetched] = useState(!!streamDetails);

    if (fetchDetails && !streamDetails && !hasFetched) {
        dispatch(fetchStreamDetails({
            networkId: accountAndNetworkScopedContext.network.id,
            transactionHash: transactionHash
        }));
        setHasFetched(true);
    }

    return (
        <>
            {
                !fetchDetails && !hasFetched && <><span> {transactionHash}: </span><button onClick={() => setFetchDetails(true)}>FETCH DETAILS</button></>
            }
            {
                fetchDetails && !streamDetails && <span> {transactionHash}: Loading...</span>
            }
            {
                streamDetails && <><span> {transactionHash}: </span><pre>{ JSON.stringify(streamDetails)}</pre></>
            }
        </>
    );
};
