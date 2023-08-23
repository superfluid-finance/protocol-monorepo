import "../styles/globals.css";
import type { AppProps } from "next/app";
import { wrapper } from "../redux/store";
import { Provider } from "react-redux";

export function MyApp({ Component, ...rest }: AppProps) {
    const { store, props: pageProps } = wrapper.useWrappedStore(rest);
    return (
        <Provider store={store}>
            <Component {...pageProps} />
        </Provider>
    );
}

export default MyApp;
