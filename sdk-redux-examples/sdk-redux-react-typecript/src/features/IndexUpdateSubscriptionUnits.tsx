import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, Switch, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";
import { IndexUpdateSubscriptionUnitsMutation } from "@superfluid-finance/sdk-redux";

export const IndexUpdateSubscriptionUnits: FC = (): ReactElement => {
    const [update, { isLoading, error }] =
        sfApi.useIndexUpdateSubscriptionUnitsMutation();

    const [chainId, signerAddress, signer] = useContext(SignerContext);
    const [superToken, setSuperToken] = useState<string>("");
    const [subscriberAddress, setSubscriberAddress] = useState<string>("");
    const [indexId, setIndexId] = useState<string>("");
    const [unitsNumber, setUnitsNumber] = useState<string>("");
    const [userDataBytes, setUserDataBytes] = useState<string>("");

    const handleOperation = (e: SyntheticEvent) => {
        update({
            chainId,
            superTokenAddress: superToken,
            indexId,
            userDataBytes,
            unitsNumber,
            subscriberAddress,
            signer
        } as IndexUpdateSubscriptionUnitsMutation);
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
                                label="Index ID"
                                onChange={(e) =>
                                    setIndexId(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="Subscriber"
                                onChange={(e) =>
                                    setSubscriberAddress(e.currentTarget.value)
                                }
                            />
                            <TextField
                                sx={{ m: 1 }}
                                label="Units"
                                onChange={(e) =>
                                    setUnitsNumber(e.currentTarget.value)
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
                                Update
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
