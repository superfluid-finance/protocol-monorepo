import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, Switch, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

export const ApproveIndexSubscription: FC = (): ReactElement => {
    const [approve, { isLoading, error }] =
        sfApi.useApproveIndexSubscriptionMutation();

    const [chainId, signerAddress] = useContext(SignerContext);
    const [superToken, setSuperToken] = useState<string>("");
    const [publisherAddress, setPublisherAddress] = useState<string>("");
    const [indexId, setIndexId] = useState<string>("");
    const [userDataBytes, setUserDataBytes] = useState<string>("");
    const [waitForConfirmation, setWaitForConfirmation] =
        useState<boolean>(false);

    const handleOperation = (e: SyntheticEvent) => {
        approve({
            waitForConfirmation,
            chainId,
            superTokenAddress: superToken,
            indexId,
            userDataBytes,
            publisherAddress,
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
                                label="SuperToken"
                                onChange={(e) =>
                                    setSuperToken(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="Publisher"
                                onChange={(e) =>
                                    setPublisherAddress(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="Index ID"
                                onChange={(e) =>
                                    setIndexId(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="User Data"
                                onChange={(e) =>
                                    setUserDataBytes(e.currentTarget.value)
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
                                onClick={handleOperation}
                            >
                                Approve
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
