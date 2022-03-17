import { FC, ReactElement } from "react";
import { Alert, Button } from "@mui/material";
import { SerializedError } from "@reduxjs/toolkit";

interface Props {
    error: SerializedError;
    retry?: () => void;
}

export const Error: FC<Props> = ({ error, retry }): ReactElement => {
    return (
        <>
            <Alert sx={{ m: 1 }} severity="error">
                {error.message}
            </Alert>
            {retry && (
                <Button variant="contained" onClick={() => retry()}>
                    Try again?
                </Button>
            )}
        </>
    );
};
