import {
    Address,
    BigNumber,
    BlockNumber,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    FlowOperator_Filter,
    FlowOperator_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    AccountAccessSettingsDocument,
    AccountAccessSettingsQuery,
    AccountAccessSettingsQueryVariables,
} from "./accountAccessSettings.generated";

export interface AccountAccessSetting {
    id: string;
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
    updatedAtTimestamp: Timestamp;

    flowOperator: string;
    sender: {
        id: Address;
    };
    token: {
        id: Address;
        decimals: number;
        symbol: string;
    };
    flowRateAllowanceRemaining: BigNumber;
    allowance: BigNumber;
    permissions: number;
}

export type AccountAccessSettingListQuery = SubgraphListQuery<
    FlowOperator_Filter,
    FlowOperator_OrderBy
>;

export class AccountAccessSettingQueryHandler extends SubgraphQueryHandler<
    AccountAccessSetting,
    AccountAccessSettingListQuery,
    AccountAccessSettingsQuery,
    AccountAccessSettingsQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof FlowOperator_Filter)[];
        tokenKeys: (keyof FlowOperator_Filter)[];
    } => ({
        accountKeys: ["sender"],
        tokenKeys: ["token"],
    });

    getRelevantAddressesFromResultCore = (
        result: AccountAccessSetting
    ): RelevantAddressesIntermediate => ({
        accounts: [result.id],
        tokens: [],
    });

    mapFromSubgraphResponse = (
        response: AccountAccessSettingsQuery
    ): AccountAccessSetting[] =>
        response.flowOperators.map((x) => ({
            ...x,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
        }));
    requestDocument = AccountAccessSettingsDocument;
}
