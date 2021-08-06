import type { Flow } from '@superfluid-finance/js-sdk'
import arrow from '../assets/arrow.svg'
import Decimal from 'decimal.js'

type DetailsProps = {
    address: string
    netFlow: string
    inFlows: Array<Flow>
    outFlows: Array<Flow>
}

export default function Details(props: DetailsProps) {

    const { address, netFlow, inFlows, outFlows } = props

    const getFlowRate = (numeric: string) => {
        return new Decimal(numeric).dividedBy(1e18).toString()
    }

    const getShortAddress = (address: string): string => {
        const start = address.substring(0, 5)
        const end = address.substring(address.length - 5, address.length)
        return `${start}..${end}`
    }

    return (
        <div className='details'>
            <div className='card-header'>
                <h3>Superfluid User Details</h3>
            </div>
            <div className='card-content'>
                <p>
                    Address:
                    <br/>
                    <span className='highlight'>{address}</span>
                </p>
                <p>
                    Net Flow:
                    <br/>
                    <span className='highlight'>{getFlowRate(netFlow)}</span>
                    fUSDCx/s
                </p>
            </div>

            <div className='card-header'>
                <p>In Flows:</p>
            </div>
            <div className='card-content'>
            {
                inFlows.length > 0 ? (
                    inFlows.map(flow => {
                        return (
                            <div className='flow' key={flow.sender}>
                                <div className='flow-addr'>
                                    <div>
                                        <p>Sender:</p>
                                        <p>{getShortAddress(flow.sender)}</p>
                                    </div>
                                    <img
                                        src={arrow}
                                        alt='arrow'
                                    />
                                    <div>
                                        <p>Receiver:</p>
                                        <p>{getShortAddress(flow.receiver)}</p>
                                    </div>
                                </div>
                                <div>
                                    <p>
                                        Flow Rate:
                                        <span className='highlight'>
                                            {getFlowRate(flow.flowRate)}
                                        </span>
                                        fUSDCx/s
                                    </p>
                                </div>
                            </div>
                        )
                    })
                ) : (
                    <p>No in-flows are active</p>
                )
            }
            </div>
            <div className='card-header'>
                <p>Out Flows:</p>
            </div>
            <div className='card-content'>
            {
                outFlows.length > 0 ? (
                    outFlows.map(flow => {
                        return (
                            <div className='flow' key={flow.receiver}>
                                <div className='flow-addr'>
                                    <div>
                                        <p>Sender:</p>
                                        <p>{getShortAddress(flow.sender)}</p>
                                    </div>
                                    <img
                                        src={arrow}
                                        alt='arrow'
                                    />
                                    <div>
                                        <p>Receiver:</p>
                                        <p>{getShortAddress(flow.receiver)}</p>
                                    </div>
                                </div>
                                <div>
                                    <p>
                                        Flow Rate:
                                        <span className='highlight'>
                                            {getFlowRate(flow.flowRate)}
                                        </span>
                                        fUSDCx/s
                                    </p>
                                </div>
                            </div>
                        )
                    })
                ) : (
                    <p>No out-flows are active</p>
                )
            }
            </div>
        </div>
    )
}