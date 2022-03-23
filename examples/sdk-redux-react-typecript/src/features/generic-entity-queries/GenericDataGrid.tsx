import { Loader } from "../../Loader";
import { Error } from "../../Error";
import { Pagination } from "@mui/material";
import { PagedResult } from "@superfluid-finance/sdk-core";
import { DataGrid, GridColDef, GridSortModel } from "@mui/x-data-grid";
import { FC, ReactElement } from "react";
import _ from "lodash";
import { SerializedError } from "@reduxjs/toolkit";

function CustomPagination(
    count: number,
    page: number,
    setPage: (page: number) => void
) {
    return (
        <Pagination
            color="primary"
            count={count}
            page={page}
            onChange={(event, value) => setPage(value)}
        />
    );
}

export interface GenericDataGridProps {
    data?: PagedResult<any>;
    isLoading: boolean;
    isFetching: boolean;
    error?: SerializedError;
    refetch?: () => void;
    pageSize: number;
    pageUseState: [number, React.Dispatch<number>];
    gridSortModelUseState: [
        GridSortModel,
        React.Dispatch<React.SetStateAction<GridSortModel>>
    ];
}

export const GenericDataGrid: FC<GenericDataGridProps> = ({
    data,
    isLoading,
    isFetching,
    error,
    refetch,
    pageSize,
    pageUseState,
    gridSortModelUseState,
}): ReactElement => {
    let columns: GridColDef[] = [];
    if (data?.data.length) {
        columns = Object.keys(data.data[0]).map((x) => ({
            field: x,
            headerName: _.startCase(x),
            flex: Object.keys(data.data[0]).length,
        }));
    }
    const [page, setPage] = pageUseState;
    const [sortModel, setSortModel] = gridSortModelUseState;

    return (
        <>
            {isLoading ? (
                <Loader />
            ) : error ? (
                <Error error={error} retry={refetch} />
            ) : (
                <></>
            )}
            {data && (
                <div style={{ height: 640, width: "100%" }}>
                    <DataGrid
                        disableColumnFilter={true}
                        pagination={true}
                        rows={data.items}
                        columns={columns}
                        sortingMode="server"
                        sortModel={sortModel}
                        onSortModelChange={(newModel) => setSortModel(newModel)}
                        paginationMode="server"
                        page={page - 1}
                        components={{
                            Pagination: () =>
                                CustomPagination(
                                    data.nextPaging ? page + 1 : page,
                                    page,
                                    (page) => setPage(page)
                                ),
                        }}
                        rowsPerPageOptions={[pageSize]}
                        onPageChange={(page) => setPage(page)}
                        rowCount={
                            data?.nextPaging
                                ? (page + 2) * pageSize
                                : (page + 1) * pageSize
                        }
                        loading={isFetching}
                    />
                </div>
            )}
        </>
    );
};
