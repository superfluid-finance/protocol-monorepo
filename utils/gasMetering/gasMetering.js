const {
  BN
} = require('@openzeppelin/test-helpers');
const { web3 } = require("@openzeppelin/test-helpers/src/setup");

const {
  GasMeterJSONReporter,
  GasMeterHTMLReporter
} = require('./gasReporter')

class Formatter {
  static formatBigNumber(key, value, fiatCurr) {
    
    switch (key) {
      case 'cost':
        value = `${web3.utils.fromWei(value)} ETH`
        break
      case 'fiatCost':
        value = `${web3.utils.fromWei(value)} ${fiatCurr}`
        break
      case 'gas':
        value = value.toString(10)
        break
      case 'gasPrice':
        value = `${web3.utils.fromWei(value, 'gwei')} GWEI`  
        break
    }
    return value
    
  }

  static formatObject(x, fiatCurr) {
    const result = {}
    Object.keys(x).forEach((key) => {
      var value = x[key]
      if(BN.isBN(value)) {
        value = Formatter.formatBigNumber(key, value, fiatCurr)
      }
      result[key] = value
    })
    return result
  }
}



module.exports = class GasMeter {

  constructor(outputFormat, gasPrice, fiatCurr, fiatPrice) {
    switch (outputFormat) {
      case 'JSON':
        this.reporter = new GasMeterJSONReporter()
        break
      case 'HTML':
        this.reporter = new GasMeterHTMLReporter()
        break
      case 'TENDERLY':
        // TODO
      default:
        throw new Error(`Unsuported report type ${outputFormat}`)
    }
    this.gasPrice = new BN(gasPrice)
    this.fiatCurr = fiatCurr || 'USD'
    this.fiatPrice = fiatPrice ? new BN(fiatPrice) : new BN('0')
    this.records = []
    this.totals = {
      gas: new BN('0'),
      gasPrice: this.gasPrice,
      cost: new BN('0'),
      fiatCost: new BN('0')
    }
  }

  _format() {
    const formattedTotals = {}
    Object.keys(this.totals).forEach(key => {
      formattedTotals[key] = Formatter.formatBigNumber(key, this.totals[key], this.fiatCurr)
    })
    const formattedRecords = this.records.map(x => { return Formatter.formatObject(x, this.fiatCurr) })
    return {
      totals: {...formattedTotals},
      executedTxs: [...formattedRecords]
    }
  }

  pushTx(tx, actionName) {
    const gas = new BN(tx.receipt.gasUsed)
    const cost = gas.mul(this.gasPrice)
    const fiatCost = cost.mul(this.fiatPrice)
    this.records.push({
      action: actionName,
      txHash: tx.tx,
      gas: gas,
      cost: cost,
      fiatCost: fiatCost
    })
    this.totals = {
      gas: this.totals.gas.add(gas),
      gasPrice: this.gasPrice,
      cost: this.totals.cost.add(cost),
      fiatCost: this.totals.fiatCost.add(fiatCost),
    }

  }

  report() {
    const formattedReport = this._format()
    console.log('formattedReport', formattedReport)
    this.reporter.generateOutput(formattedReport)
  }

}