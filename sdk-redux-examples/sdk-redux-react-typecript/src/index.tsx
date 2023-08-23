import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App";
import { store } from "./redux/store";
import { Provider } from "react-redux";
import theme from "./theme";
import { CssBaseline, ThemeProvider } from "@mui/material";
import { WagmiConfig } from "wagmi";
import { chains, wagmiClient } from "./wagmiAndRainbowKit";
import { RainbowKitProvider } from "@rainbow-me/rainbowkit";
import "@rainbow-me/rainbowkit/styles.css";

const root = ReactDOM.createRoot(document.getElementById("root")!);

const Root = () => {
    return (
        <React.StrictMode>
            <ThemeProvider theme={theme}>
                {/* CssBaseline kickstart an elegant, consistent, and simple baseline to build upon. */}
                <CssBaseline />
                <Provider store={store}>
                    <WagmiConfig client={wagmiClient}>
                        <RainbowKitProvider chains={chains}>
                            <App />
                        </RainbowKitProvider>
                    </WagmiConfig>
                </Provider>
            </ThemeProvider>
        </React.StrictMode>
    );
};

root.render(<Root />);
