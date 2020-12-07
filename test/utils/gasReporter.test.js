const {
    GasMeterHTMLReporter
} = require("../../utils/gasMetering/gasReporter");
const { expect } = require("chai");

describe("Gas Reporting", function () {

    beforeEach(async () => {
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
        const htmlResult = `<html><head><styletype="text/css">body{font-family:"OpenSans",sans-serif;line-height:1.25;}table{border:1pxsolid#ccc;border-collapse:collapse;margin:0;padding:0;width:100%;table-layout:fixed;}tablecaption{font-size:1.5em;margin:.5em0.75em;}tabletr{background-color:#f8f8f8;border:1pxsolid#ddd;padding:.35em;}tableth,tabletd{padding:.625em;text-align:center;max-width:100px;overflow:hidden;text-overflow:ellipsis;}tableth{font-size:.85em;letter-spacing:.1em;text-transform:uppercase;}@mediascreenand(max-width:600px){table{border:0;}tablecaption{font-size:1.3em;}tablethead{border:none;clip:rect(0000);height:1px;margin:-1px;overflow:hidden;padding:0;position:absolute;width:1px;}tabletr{border-bottom:3pxsolid#ddd;display:block;margin-bottom:.625em;}tabletd{border-bottom:1pxsolid#ddd;display:block;font-size:.8em;text-align:right;}tabletd::before{/**aria-labelhasnoadvantage,itwon'tbereadinsideatablecontent:attr(aria-label);*/content:attr(data-label);float:left;font-weight:bold;text-transform:uppercase;}tabletd:last-child{border-bottom:0;}}</style></head><body><h2>gasReport</h2><divclass="table-wrapper"><tableclass="fl-table"><thead><tr><tr><th>action</th><th>txHash</th><th>gas</th><th>cost</th><th>fiatCost</th></tr></tr></thead><tbody><tr><td>SomeAction</td><td>0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286</td><td>100000</td><td>0.0001ETH</td><td>0.04USD</td></tr><tr><td>SomeAction2</td><td>0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286</td><td>100000</td><td>0.0001ETH</td><td>0.04USD</td></tr><tbody></table></div><h2>Stats</h2><divclass="table-wrapper"><tableclass="fl-table"><thead><tr><tr><th>gas</th><th>gasPrice</th><th>cost</th><th>fiatCost</th></tr></tr></thead><tbody><tr><td>200000</td><td>1GWEI</td><td>0.0002ETH</td><td>0.08USD</td></tr><tbody></table></div></body></html>
        `.replace(/\s+/g,"").replace(/ /g,"").replace(/\n/g,"");
        /* eslint-enable */
        expect(this.writeFileSyncArg.path).to.equal(process.cwd()+"/build/gasReport.html");
        expect(this.writeFileSyncArg.result.replace(/\s+/g,"").replace(/ /g,"").replace(/\n/g,"")).to.equal(htmlResult);

    });

});
