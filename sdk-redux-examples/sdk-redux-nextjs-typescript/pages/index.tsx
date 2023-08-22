import Head from "next/head";
import Link from "next/link";
import Layout, {siteTitle} from "../components/layout";
import utilStyles from "../styles/utils.module.css";

export default function Home({}) {
    return (
        <Layout home>
            <Head>
                <title>{siteTitle}</title>
            </Head>
            <section
                className={`${utilStyles.headingMd} ${utilStyles.padding1px}`}
            >
                <h2>Look at some SuperTokens</h2>
                <ul>
                    <li>
                        fDAIx <Link href="/tokens/fdaix">dynamic</Link>{" "}
                        <Link href="/tokens/ssr/fdaix">SSR</Link>{" "}
                        <Link href="/tokens/ssg/fdaix">SSG</Link>
                    </li>
                    <li>
                        ETHx <Link href="/tokens/ethx">dynamic</Link>{" "}
                        <Link href="/tokens/ssr/ethx">SSR</Link>{" "}
                        <Link href="/tokens/ssg/ethx"> fallback SSG</Link>
                    </li>
                </ul>
            </section>
        </Layout>
    );
}

// Fun fact: exporting this function enables SSG.
export async function getStaticProps() {
    return {
        props: {},
    };
}
