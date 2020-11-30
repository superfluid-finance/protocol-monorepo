const { expect } = require('chai')
const {
  BN
} = require('@openzeppelin/test-helpers');
const {
  GasMeterJSONReporter,
  GasMeterHTMLReporter
} = require('./gasReporter')

describe('GasMetering', function () {

  beforeEach(async () => {
    this.result = {
      totals: {
        gas: '200000',
        gasPrice: '1 GWEI' ,
        cost: '0.0002 ETH',
        fiatCost: '0.08 USD'
      },
      executedTxs: [{
        action: 'SomeAction',
        txHash: '0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286',
        gas: '100000',
        cost: '0.0001 ETH',
        fiatCost: '0.04 USD'
      },
      {
        action: 'SomeAction2',
        txHash: '0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286',
        gas: '100000',
        cost: '0.0001 ETH',
        fiatCost: '0.04 USD'
      }]
    }

    
  })

  it('should generate correct JSON table transaction log', async () => {


  })

  it('should generate correct HTML table transaction log', async () => {
    const reporter = GasMeterHTMLReporter()

  })



 

})
