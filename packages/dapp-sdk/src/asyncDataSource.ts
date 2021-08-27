import {Account, AccountCompositeKey, StreamIndex} from "./Account";
import {StreamCompositeKey, StreamDetails} from "./Network";

const allStreamDetails: Array<StreamDetails> =
    [
        {
            networkId: 137,
            transactionHash: "transactionHash1",
            fromAddress: "address1",
            toAddress: "address3",
            isActive: true
        },
        {
            networkId: 137,
            transactionHash: "transactionHash2",
            fromAddress: "address3",
            toAddress: "address1",
            isActive: false
        },
        {
            networkId: 137,
            transactionHash: "transactionHash3",
            fromAddress: "address1",
            toAddress: "DOES_NOT_EXIST_ADDRESS",
            isActive: false
        }
    ]

const allAccounts: Array<Account> =
    [
        {
            accountAddress: 'address1',
            networkId: 137,
        },
        {
            accountAddress: 'address2',
            networkId: 421611,
        },
        {
            accountAddress: 'address3',
            networkId: 137,
        }
    ]

export const asyncMockDataSource = {
    fetchAccount(arg: AccountCompositeKey): Promise<Account | undefined> {
        return new Promise<Account | undefined>((resolve) => {
            setTimeout(() => {
                resolve(allAccounts.find(x => x.accountAddress === arg.accountAddress && x.networkId === arg.networkId));
            }, 2000)
        })
    },

    fetchAccountStreamIndexes(networkId: number, accountAddress: string, isActive: boolean): Promise<StreamIndex[]> {
        // TODO(KK): What if network or account doesn't exist? I think I should throw.
        return new Promise<StreamIndex[]>((resolve) => {
            setTimeout(() => {
                const filteredStreams = allStreamDetails.filter(x => x.networkId === networkId && x.isActive === isActive && (x.fromAddress === accountAddress || x.toAddress === accountAddress));
                const mappedStreams = filteredStreams.map<StreamIndex>((value) => {
                    return { transactionHash: value.transactionHash}
                });
                resolve(mappedStreams);
            }, 2000)
        })
    },

    fetchStreamDetails(arg: StreamCompositeKey): Promise<StreamDetails | undefined> {
        return new Promise<StreamDetails | undefined>((resolve) => {
            setTimeout(() => {
                resolve(allStreamDetails.find(x => x.networkId === arg.networkId && x.transactionHash === arg.transactionHash));
            }, 2000)
        })
    }
};
