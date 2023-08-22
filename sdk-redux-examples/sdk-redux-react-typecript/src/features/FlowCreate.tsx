import { SignerContext } from "../SignerContext";
import { Loader } from "../Loader";
import { FC, ReactElement, SyntheticEvent, useContext, useState } from "react";
import { Button, FormGroup, Switch, TextField } from "@mui/material";
import { Error } from "../Error";
import { sfApi } from "../redux/store";
import { FlowCreateMutation } from "@superfluid-finance/sdk-redux";

export const FlowCreate: FC = (): ReactElement => {
    const [createFlow, { isLoading, error }] = sfApi.useFlowCreateMutation();

    const [chainId, signerAddress, signer] = useContext(SignerContext);

    const [receiver, setReceiver] = useState<string>("");
    const [superToken, setSuperToken] = useState<string>("");
    const [flowRate, setFlowRate] = useState<string>("");
    const [userDataBytes, setUserDataBytes] = useState<string>("");

    const handleCreateStream = (e: SyntheticEvent) => {
        createFlow({
            senderAddress: signerAddress,
            receiverAddress: receiver,
            flowRateWei: flowRate,
            chainId,
            superTokenAddress: superToken,
            userDataBytes,
            signer
        } as FlowCreateMutation);
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
                            <TextField
                                sx={{ m: 1 }}
                                label="Flow Rate"
                                type="number"
                                onChange={(e) =>
                                    setFlowRate(e.currentTarget.value)
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
                                onClick={handleCreateStream}
                            >
                                Create
                            </Button>
                        </FormGroup>
                    </form>
                </>
            )}
        </>
    );
};
