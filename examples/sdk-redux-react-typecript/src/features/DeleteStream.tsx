import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, Switch, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

export const DeleteStream: FC = (): ReactElement => {
    const [deleteFlow, { isLoading, error }] = sfApi.useDeleteFlowMutation();

    const [chainId, signerAddress] = useContext(SignerContext);

    const [receiver, setReceiver] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [waitForConfirmation, setWaitForConfirmation] =
        useState<boolean>(false);

    const handleDeleteStream = (e: SyntheticEvent) => {
        deleteFlow({
            senderAddress: signerAddress,
            receiverAddress: receiver,
            chainId,
            superTokenAddress: superToken,
            waitForConfirmation,
        });
    };

    return (
        <>
            {isLoading ? (
                <Loader />
            ) : (
                <>
                    {error && <Error error={error} />}
                    <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                        <FormGroup>
                            <TextField
                                sx={{ m: 1 }}
                                label="Receiver"
                                onChange={(e) =>
                                    setReceiver(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="SuperToken"
                                onChange={(e) =>
                                    setSuperToken(e.currentTarget.value)
                                }
                            />
                            <Switch
                                value={waitForConfirmation}
                                title="Wait for confirmation"
                                onChange={() =>
                                    setWaitForConfirmation(!waitForConfirmation)
                                }
                            />
                            <Button
                                sx={{ m: 1 }}
                                type="submit"
                                variant="contained"
                                fullWidth={true}
                                onClick={handleDeleteStream}
                            >
                                Delete
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
