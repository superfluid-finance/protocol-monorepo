import { useFlash } from '@redwoodjs/web'
import { navigate, routes } from '@redwoodjs/router'
import FlowForm from 'src/components/FlowForm'
import { useAuth } from '@redwoodjs/auth'

import { Web3Provider } from '@ethersproject/providers'
import SuperfluidSDK from '@superfluid-finance/js-sdk'

const NewFlow = ({ to }) => {
  const { currentUser } = useAuth()
  const { addMessage } = useFlash()

  const [error, setError] = React.useState(null)
  const [loading, setLoading] = React.useState(false)

  const newFlow = async (input) => {
    setLoading(true)

    const sf = new SuperfluidSDK.Framework({
      ethers: new Web3Provider(window.ethereum),
    })
    await sf.initialize()
    const owner = sf.user({
      address: input.ownerAddress,
      token: input.tokenAddress,
    })
    const tx = await owner.flow({
      recipient: input.recipientAddress,
      flowRate: input.flowRate,
    })
    await tx.wait()

    setLoading(false)
    addMessage('Flow created.', { classes: 'rw-flash-success' })
    navigate(
      routes.flow({ from: input.ownerAddress, to: input.recipientAddress })
    )
  }

  return (
    <>
      <div className="rw-segment">
        <header className="rw-segment-header">
          <h2 className="rw-heading rw-heading-secondary">New Flow</h2>
        </header>
        <div className="rw-segment-main">
          <FlowForm
            flow={{
              recipientAddress: to,
              ownerAddress: currentUser.address,
            }}
            onSave={newFlow}
            loading={loading}
            error={error}
          />
        </div>
      </div>
      <div className="mt-4">
        <h3>
          Goerli token addresses (
          <a
            className="text-blue"
            href="https://docs.superfluid.finance/superfluid/resources/networks"
          >
            view all
          </a>
          )
        </h3>
        <ul className="mt-4">
          <li>
            fDAIx <b>0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00</b>
          </li>
          <li>
            fUSDCx <b>0x8aE68021f6170E5a766bE613cEA0d75236ECCa9a</b>
          </li>
          <li>
            fTUSDx <b>0x95697ec24439E3Eb7ba588c7B279b9B369236941</b>
          </li>
        </ul>
      </div>
    </>
  )
}

export default NewFlow
