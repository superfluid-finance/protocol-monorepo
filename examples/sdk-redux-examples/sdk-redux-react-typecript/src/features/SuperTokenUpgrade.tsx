import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, TextField, Switch } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";

export const SuperTokenUpgrade: FC = (): ReactElement => {
    const [upgradeToSuperToken, { isLoading, error }] =
        sfApi.useSuperTokenUpgradeMutation();

    const [chainId, signerAddress] = useContext(SignerContext);

    const [amount, setAmount] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [waitForConfirmation, setWaitForConfirmation] =
        useState<boolean>(false);

    const {
        data: availableAllowance,
        isFetching: isAllowanceFetching,
        isError: isAllowanceQueryError,
    } = sfApi.useSuperTokenUpgradeAllowanceQuery(
        {
            accountAddress: signerAddress,
            superTokenAddress: superToken,
            chainId: chainId,
        },
        {
            skip: !superToken,
        }
    );

    const handleUpgradeToSuperToken = (e: SyntheticEvent) => {
        upgradeToSuperToken({
            waitForConfirmation,
            chainId,
            superTokenAddress: superToken,
            amountWei: amount,
        });
    };

    return (
        <>
            {isLoading ? (
                <Loader />
            ) : (
                <>
                    {error && <Error error={error} />}
                    {availableAllowance &&
                        !isAllowanceFetching &&
                        !isAllowanceQueryError && (
                            <p>Available allowance: {availableAllowance}</p>
                        )}
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
                                onClick={handleUpgradeToSuperToken}
                            >
                                Upgrade
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
