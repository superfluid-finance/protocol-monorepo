import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useState,
} from "react";
import {
    FormGroup,
    TextField,
} from "@mui/material";
import { SignerContext } from "../../SignerContext";
import { sfSubgraph } from "../../redux/store";
import { Loader } from "../../Loader";
import { Error } from "../../Error";

export const Account: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);
    const [accountAddress, setAccountAddress] = useState<string>(signerAddress);

    const {data, isLoading, error, refetch } = sfSubgraph.useAccountQuery({
        chainId: queryChainId,
        id: accountAddress,
    });

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <TextField
                        sx={{ m: 1 }}
                        label="Chain ID"
                        value={queryChainId}
                        onChange={(e) =>
                            setQueryChainId(Number(e.currentTarget.value))
                        }
                    />
                    <TextField
                        sx={{ m: 1 }}
                        label="Address"
                        value={accountAddress}
                        onChange={(e) =>
                            setAccountAddress(e.currentTarget.value)
                        }
                    />
                </FormGroup>
            </form>
            {isLoading ? (
                <Loader />
            ) : error ? (
                <Error error={error} retry={refetch} />
            ) : (
                <></>
            )}
            {
                data && <pre>{JSON.stringify(data, null, 2)}</pre>
            }
        </>
    );
};
