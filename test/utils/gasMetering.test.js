const { expect } = require('chai')
const {
  BN
} = require('@openzeppelin/test-helpers');
const GasMetering = require('../../utils/gasMetering/gasMetering')

describe('GasMetering', function () {

  beforeEach(async () => {
    this.tx = {
      tx: '0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286',
      receipt: {
        transactionHash: '0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286',
        transactionIndex: 0,
        blockHash: '0x362333884391810d9a675c8639b4062abeb990604b25ff9ef3db6199d8901874',
        blockNumber: 35,
        from: '0xf17f52151ebef6c7334fad080c5704d77216b732',
        to: '0x9fbda871d559710256a2502a2517b794b482db40',
        gasUsed: 100000,
        cumulativeGasUsed: 100000,
        contractAddress: null,
        logs: [],
        status: true,
        logsBloom: '0x00000000000000000000010000000000000000000000001000000000000000000000000000000020008000800000000008000000000000000000000001000000000000000000800000000000000000000000000400000080000080000000000000000000000000000000000200000000000000000000000000000000000000000000000200010000000000000000000000000000000000000200000100000000020000000000000000000000000000000000000100000000000000000000000000000000000000001000000000000000020000000000000004000000000000000000020040400000000000000000000000004000000800000002000000000000',
        rawLogs: [ [{}] ]
      },
      logs: []
    }
    
    this.gasPrice = '1000000000' // 1gwei
  })

  it('should add transaction log', async () => {
    const gasMeter = new GasMetering('JSON', this.gasPrice, 'USD', '400')

    gasMeter.pushTx(this.tx, 'SomeAction')
    const record = gasMeter.records[0]
    expect(record.action).to.be.equal('SomeAction')
    expect(record.txHash).to.be.equal(this.tx.tx)
    expect(record.gas).to.be.bignumber.equal(new BN(100000))
    const expectedCost = new BN(100000).mul(new BN(this.gasPrice))
    expect(record.cost).to.be.bignumber.equal(expectedCost)
    expect(record.fiatCost).to.be.bignumber.equal(expectedCost.mul(new BN('400')))

  })

  it('should calculate totals', async () => {
    const gasMeter = new GasMetering('JSON', this.gasPrice, 'USD', '400')

    gasMeter.pushTx(this.tx, 'SomeAction')
    gasMeter.pushTx(this.tx, 'SomeAction2')
    const totals = gasMeter.totals
    expect(totals.gas).to.be.bignumber.equal(new BN(200000))
    const expectedCost = new BN(200000).mul(new BN(this.gasPrice))
    expect(totals.cost).to.be.bignumber.equal(expectedCost)
    expect(totals.fiatCost).to.be.bignumber.equal(expectedCost.mul(new BN('400')))
    expect(totals.gasPrice).to.be.bignumber.equal(new BN(this.gasPrice))

  })

  it('should format correctly', async () => {
    const gasMeter = new GasMetering('JSON', this.gasPrice, 'USD', '400')

    gasMeter.pushTx(this.tx, 'SomeAction')
    gasMeter.pushTx(this.tx, 'SomeAction2')
    const result = gasMeter._format()

    expect(result).to.be.deep.equal({
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
    })

  })


})
