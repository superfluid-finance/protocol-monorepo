import { SignerContext } from "../SignerContext";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Alert, Button, FormGroup, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfSubgraph } from "../redux/store";

export const MonitorForEventsToInvalidateCache: FC = (): ReactElement => {
    const [trigger, { error, isSuccess }] =
        sfSubgraph.useMonitorForEventsToInvalidateCacheMutation();

    const [chainId, signerAddress] = useContext(SignerContext);
    const [address, setAddress] = useState<string>(signerAddress);
    const [unsubscribe, setUnsubscribe] = useState<() => void>(() => () => {});

    const handleButtonClick = (e: SyntheticEvent) => {
        unsubscribe();
        const mutationResult = trigger({
            chainId,
            address: address,
        });
        setUnsubscribe(() => mutationResult.unsubscribe);
    };

    return (
        <>
            {isSuccess && <Alert severity="success">Monitoring! (Every ~20 seconds polling for changes to the blockchain. If you close this component then it stops.)</Alert>}
            {error && <Error error={error} />}
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <TextField
                        sx={{ m: 1 }}
                        label="Address"
                        onChange={(e) => setAddress(e.currentTarget.value)}
                    />
                    <Button
                        sx={{ m: 1 }}
                        type="submit"
                        variant="contained"
                        fullWidth={true}
                        onClick={handleButtonClick}
                    >
                        Monitor
                    </Button>
                </FormGroup>
            </form>
        </>
    );
};
