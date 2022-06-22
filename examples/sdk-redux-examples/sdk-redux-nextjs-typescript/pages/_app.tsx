import "../styles/globals.css";
const {wrapper} = require("../redux/store");
import type {AppProps} from "next/app";

export function MyApp({Component, pageProps}: AppProps) {
    return <Component {...pageProps} />;
}

export default wrapper.withRedux(MyApp);
