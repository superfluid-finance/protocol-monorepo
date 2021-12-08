const fs = require("fs");
const path = require("path");
class GasMeterReporter {
    constructor({fileSystem, fileName}) {
        this.fs = fileSystem ? fileSystem : fs;
        this.filePath = path.join(process.cwd(), "reports");
        if (!fs.existsSync(this.filePath)) {
            fs.mkdirSync(this.filePath);
        }
        this.fileName = fileName ? fileName : "gasReport";
    }
}

class GasMeterJSONReporter extends GasMeterReporter {
    generateOutput(report) {
        const result = JSON.stringify(report);
        this.fs.writeFileSync(`${this.filePath}${this.fileName}.json`, result);
    }
}

class GasMeterHTMLReporter extends GasMeterReporter {
    _joinStrings(prevVal, currVal, idx) {
        return idx == 0 ? currVal : prevVal + currVal;
    }

    _generateHeaders(element) {
        let keys = Object.keys(element);

        const headers = keys
            .map((key) => {
                return `<th>${key}</th>`;
            })
            .reduce(this._joinStrings);

        return `<tr>${headers}</tr>`;
    }

    _generateBody(elements) {
        return elements
            .map((tx) => {
                let keys = Object.keys(tx);
                let tds = keys
                    .map((key) => {
                        return `<td>${tx[key]}</td>`;
                    })
                    .reduce(this._joinStrings);
                return `<tr>${tds}</tr>`;
            })
            .reduce(this._joinStrings);
    }

    generateReport(report) {
        const htmlStub = require("./htmlStub");
        const statHeaders = this._generateHeaders(
            Object.values(report.aggregates)[0]
        );
        const statBody = this._generateBody(Object.values(report.aggregates));
        const txHeaders = this._generateHeaders(report.executedTxs[0]);
        const txTableBody = this._generateBody(report.executedTxs);
        const result = htmlStub
            .replace("{{HEADERS-TX}}", txHeaders)
            .replace("{{BODY-TX}}", txTableBody)
            .replace("{{HEADERS-STATS}}", statHeaders)
            .replace("{{BODY-STATS}}", statBody)
            .replace("{{TITLE}}", this.fileName);

        this.fs.writeFileSync(
            path.join(this.filePath, `${this.fileName}.html`),
            result
        );
    }
}

module.exports = {
    GasMeterJSONReporter,
    GasMeterHTMLReporter,
};
