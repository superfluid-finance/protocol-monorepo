import React, { FC, ReactElement } from "react";
import { Typography } from "@mui/material";

export const Loader: FC = (): ReactElement => {
    return (
        <Typography variant="h4" component="h4">
            Loading...
        </Typography>
    );
};
