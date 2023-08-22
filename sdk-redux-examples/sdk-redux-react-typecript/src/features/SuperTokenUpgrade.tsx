import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, TextField, Switch } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";
import { SuperTokenUpgradeMutation } from "@superfluid-finance/sdk-redux";

export const SuperTokenUpgrade: FC = (): ReactElement => {
    const [upgradeToSuperToken, { isLoading, error }] =
        sfApi.useSuperTokenUpgradeMutation();

    const [chainId, signerAddress, signer] = useContext(SignerContext);

    const [amount, setAmount] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");

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
            chainId,
            superTokenAddress: superToken,
            amountWei: amount,
            signer
        } as SuperTokenUpgradeMutation);
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
