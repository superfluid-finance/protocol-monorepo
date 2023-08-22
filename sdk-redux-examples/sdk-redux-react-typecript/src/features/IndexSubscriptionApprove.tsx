import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, Switch, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";
import { IndexSubscriptionApproveMutation } from "@superfluid-finance/sdk-redux";

export const IndexSubscriptionApprove: FC = (): ReactElement => {
    const [approve, { isLoading, error }] =
        sfApi.useIndexSubscriptionApproveMutation();

    const [chainId, signerAddress, signer] = useContext(SignerContext);
    const [superToken, setSuperToken] = useState<string>("");
    const [publisherAddress, setPublisherAddress] = useState<string>("");
    const [indexId, setIndexId] = useState<string>("");
    const [userDataBytes, setUserDataBytes] = useState<string>("");

    const handleOperation = (e: SyntheticEvent) => {
        approve({
            chainId,
            superTokenAddress: superToken,
            indexId,
            userDataBytes,
            publisherAddress,
            signer
        } as IndexSubscriptionApproveMutation);
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
