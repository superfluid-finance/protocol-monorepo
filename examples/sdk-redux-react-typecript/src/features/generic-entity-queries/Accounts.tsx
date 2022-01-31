import React, {
    FC,
    ReactElement,
    SyntheticEvent,
    useContext,
    useEffect,
    useState,
} from "react";
import {
    FormControl,
    FormControlLabel,
    FormGroup,
    Radio,
    RadioGroup,
    TextField,
} from "@mui/material";
import { SignerContext } from "../../SignerContext";
import { sfSubgraph } from "../../redux/store";
import { GridSortModel } from "@mui/x-data-grid";
import { GenericDataGrid } from "./GenericDataGrid";
import { Account_OrderBy } from "@superfluid-finance/sdk-core";

export const Accounts: FC = (): ReactElement => {
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
              orderBy: sortModel[0].field as Account_OrderBy,
              orderDirection: sortModel[0].sort!,
          }
        : undefined;

    const [isSuperApp, setIsSuperApp] = useState<boolean | undefined>();

    const queryResult = sfSubgraph.useAccountsQuery({
        chainId: queryChainId,
        filter: {
            isSuperApp: isSuperApp,
        },
        pagination: {
            skip: (page - 1) * pageSize,
            take: pageSize,
        },
        order,
    });

    const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        setIsSuperApp((event.target as HTMLInputElement).value === "true");
    };

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
                    <FormControl component="fieldset">
                        <RadioGroup
                            row
                            name="row-radio-buttons-group"
                            value={isSuperApp}
                            onChange={handleChange}
                        >
                            <FormControlLabel
                                value="true"
                                control={<Radio />}
                                label="SuperApp"
                            />
                            <FormControlLabel
                                value="false"
                                control={<Radio />}
                                label="Not SuperApp"
                            />
                        </RadioGroup>
                    </FormControl>
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
