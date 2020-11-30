const fs = require('fs')

class GasMeterReporter {

  constructor(fileSystem) {
    this.fs = fileSystem ? fileSystem : fs
    this.filePath = '../../build/gasReport'
  }

}

class GasMeterJSONReporter extends GasMeterReporter {

  generateOutput(report) {
    const result = JSON.stringify(report)
    this.fs.writeFileSync(`${this.filePath}.json`, result); 
  }
}

class GasMeterHTMLReporter {

  _joinStrings(prevVal, currVal, idx) {
    return idx == 0 ? currVal : prevVal + currVal;
  }

  _generateHeaders(report) {
    let keys = Object.keys(report.executedTxs[0])
    
    const headers = keys.map(key => {
      return `<th>${key}</th>`
    }).reduce(this._joinStrings)

    return `<tr>${headers}</tr>`
  }

  _generateBody(report) {
    return report.executedTxs.map(tx => {
      let keys = Object.keys(tx)
      let tds = keys.map( key => {
        return `<td>${tx[key]}</td>`
      }).reduce(this._joinStrings)
      return `<tr>${tds}</tr>`
    }).reduce(this._joinStrings)
  }

  generateOutput(report) {
    const htmlStub = this.fs.readFileSync('./htmlStub.txt')
    const headers = this._generateHeaders(report)
    const withHeaders = htmlStub.replace('{{HEADERS}}', headers)
    const tableBody = this._generateBody(report)
    const result = withHeaders.replace('{{BODY}}', tableBody)
    console.log(result)
    this.fs.writeFileSync(`${this.filePath}.html`, result)

  }

}

module.exports = {
  GasMeterJSONReporter,
  GasMeterHTMLReporter
}