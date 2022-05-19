import React from "react";
import ReactDOM from "react-dom";
import App from "./App";
import { store } from "./redux/store";
import { Provider } from "react-redux";
import theme from "./theme";
import { CssBaseline, ThemeProvider } from "@mui/material";

ReactDOM.render(
    <React.StrictMode>
        <ThemeProvider theme={theme}>
            {/* CssBaseline kickstart an elegant, consistent, and simple baseline to build upon. */}
            <CssBaseline />
            <Provider store={store}>
                <App />
            </Provider>
        </ThemeProvider>
    </React.StrictMode>,
    document.getElementById("root")
);
