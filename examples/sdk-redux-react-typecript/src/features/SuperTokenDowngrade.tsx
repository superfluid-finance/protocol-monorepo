import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, TextField, Switch } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

export const SuperTokenDowngrade: FC = (): ReactElement => {
    const [downgradeFromSuperToken, { isLoading, error }] =
        sfApi.useSuperTokenDowngradeMutation();

    const [chainId, signerAddress] = useContext(SignerContext);

    const [amount, setAmount] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [waitForConfirmation, setWaitForConfirmation] =
        useState<boolean>(false);

    const handleDowngradeFromSuperToken = (e: SyntheticEvent) => {
        downgradeFromSuperToken({
            chainId,
            superTokenAddress: superToken,
            amountWei: amount,
            waitForConfirmation
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
                                label="Amount"
                                onChange={(e) =>
                                    setAmount(e.currentTarget.value)
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
                                onClick={handleDowngradeFromSuperToken}
                            >
                                Downgrade
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
