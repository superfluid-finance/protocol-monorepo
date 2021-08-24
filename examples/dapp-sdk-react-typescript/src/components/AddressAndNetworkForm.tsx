import React, {ReactElement, FC, useState, FormEvent} from "react";
import dappSdk from "./../dappSdk";

interface Props {
    onSubmitted: (networkId: string, accountAddress: string) => void;
}

const AddressAndNetworkForm: FC<Props> = ({onSubmitted}): ReactElement => {
    const [networkId, setNetworkId] = useState<string>("");
    const [accountAddress, setAccountAddress] = useState<string>("");
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
        event.preventDefault();
        setError(null);
        dappSdk.subscribe(networkId, accountAddress)
            .then(() => onSubmitted(networkId, accountAddress))
            .catch(x => setError(x));
    }

    return (
        <>
            {!!error && <p>{error}</p>}
            <form onSubmit={handleSubmit}>
                <label>
                    Network:
                    <input type="text" value={networkId} onChange={e => setNetworkId(e.target.value)}/>
                </label>
                <label>
                    Address:
                    <input type="text" value={accountAddress} onChange={e => setAccountAddress(e.target.value)}/>
                </label>
                <input type="submit" value="Set"/>
            </form>
        </>
    );
};

export default AddressAndNetworkForm;
