const {
    GasMeterHTMLReporter
} = require("../../utils/gasMetering/gasReporter");
const { expect } = require("chai");

describe("Gas Reporting", function () {

    beforeEach(async () => {
        this.readFileSyncArgs = null;
        this.writeFileSyncArg = {
            path: null,
            result: null
        };
        /* eslint-disable */
        this.htmlStub = `<body>
            <h2>Superfluid Gas Report</h2>
            <div class="table-wrapper">
                <table class="fl-table">
                    <thead>
                    <tr>
                        {{HEADERS-TX}}
                    </tr>
                    </thead>
                    <tbody>
                        {{BODY-TX}}
                    <tbody>
                </table>
            </div>
            <h2>Stats</h2>
            <div class="table-wrapper">
                <table class="fl-table">
                    <thead>
                    <tr>
                        {{HEADERS-STATS}}
                    </tr>
                    </thead>
                    <tbody>
                        {{BODY-STATS}}
                    <tbody>
                </table>
            </div>
        </body>`
        /* eslint-enable <rule> */
        this.mockFs = {
            readFileSync: (arg) => {
                this.readFileSyncArgs = arg;
                return this.htmlStub;
            },
            writeFileSync: (path, result) => {
                this.writeFileSyncArg.path = path;
                this.writeFileSyncArg.result = result;
            }
        };
        this.formattedReport = {
            totals: {
                gas: "200000",
                gasPrice: "1 GWEI",
                cost: "0.0002 ETH",
                fiatCost: "0.08 USD"
            },
            executedTxs: [{
                action: "SomeAction",
                txHash: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
                gas: "100000",
                cost: "0.0001 ETH",
                fiatCost: "0.04 USD"
            },
            {
                action: "SomeAction2",
                txHash: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
                gas: "100000",
                cost: "0.0001 ETH",
                fiatCost: "0.04 USD"
            }]
        };


    });


    it("should generate correct HTML table transaction log", async () => {
        const reporter = new GasMeterHTMLReporter({
            fileSystem: this.mockFs,
            fileName: 'gasReport'
        });
        reporter.generateReport(this.formattedReport);
        /* eslint-disable */
        const htmlResult = `<body><h2>SuperfluidGasReport</h2><divclass=\"table-wrapper\">
        <tableclass=\"fl-table\"><thead><tr><tr><th>action</th><th>txHash</th>
        <th>gas</th><th>cost</th><th>fiatCost</th></tr></tr></thead><tbody><tr><td>SomeAction</td>
        <td>0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286</td><td>100000</td>
        <td>0.0001ETH</td><td>0.04USD</td></tr>
        <tr><td>SomeAction2</td><td>0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286</td><td>100000</td><td>0.0001ETH</td>
        <td>0.04USD</td></tr><tbody></table></div><h2>Stats</h2>
        <divclass=\"table-wrapper\"><tableclass=\"fl-table\"><thead><tr><tr><th>gas</th>
        <th>gasPrice</th><th>cost</th><th>fiatCost</th></tr></tr>
        </thead><tbody><tr><td>200000</td><td>1GWEI</td><td>0.0002ETH</td><td>0.08USD</td>
        </tr><tbody></table></div></body>`.replace(/\s+/g,"").replace(/ /g,"").replace(/\n/g,"");
        /* eslint-enable */
        expect(this.readFileSyncArgs).to.equal("./htmlStub.txt");
        expect(this.writeFileSyncArg.path).to.equal("../../build/gasReport.html");
        expect(this.writeFileSyncArg.result.replace(/\s+/g,"").replace(/ /g,"").replace(/\n/g,"")).to.equal(htmlResult);

    });

});
