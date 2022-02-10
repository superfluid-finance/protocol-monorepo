import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import { FormGroup, TextField } from "@mui/material";
import { SignerContext } from "../../SignerContext";
import { sfSubgraph } from "../../redux/store";
import { GridSortModel } from "@mui/x-data-grid";
import { GenericDataGrid } from "./GenericDataGrid";
import { IndexSubscription_OrderBy } from "@superfluid-finance/sdk-core";

export const IndexSubscriptions: FC = (): ReactElement => {
    const [chainId, signerAddress] = useContext(SignerContext);
    const [page, setPage] = useState<number>(1);
    const [pageSize, setPageSize] = useState<number>(10);
    const [queryChainId, setQueryChainId] = useState<number>(chainId);

    useEffect(() => {
        setPage(1);
    }, [queryChainId]);

    const [sortModel, setSortModel] = React.useState<GridSortModel>([]);

    const order = !!sortModel[0]
        ? {
              orderBy: sortModel[0].field as IndexSubscription_OrderBy,
              orderDirection: sortModel[0].sort!,
          }
        : undefined;

    const queryResult = sfSubgraph.useIndexSubscriptionsQuery({
        chainId: queryChainId,
        filter: {},
        pagination: {
            skip: (page - 1) * pageSize,
            take: pageSize,
        },
        order,
    });

    return (
        <>
            <form onSubmit={(e: SyntheticEvent) => e.preventDefault()}>
                <FormGroup>
                    <TextField
                        sx={{ m: 1 }}
                        label="Chain ID"
                        value={queryChainId}
                        onChange={(e) =>
                            setQueryChainId(Number(e.currentTarget.value))
                        }
                    />
                </FormGroup>
            </form>
            <GenericDataGrid
                {...queryResult}
                pageSize={pageSize}
                pageUseState={[page, setPage]}
                gridSortModelUseState={[sortModel, setSortModel]}
            />
        </>
    );
};
