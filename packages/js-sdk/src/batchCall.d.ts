// TODO set batchCall return type
import type { Transaction } from 'web3-core'

// ERC20 Approve/TransferFrom
type ERC20OperationType = 'ERC20_APPROVE'| 'ERC20_TRANSFER_FROM' | 1 | 2;
interface ERC20Operation {
    type: ERC20OperationType
    data: {
        token: string
        amount: string
        spender: string
    }
}

// SuperToken Upgrade/Downgrade
type SuperTokenOperationType = 'SUPERTOKEN_UPGRADE' | 'SUPERTOKEN_DOWNGRADE' | 101 | 102;
interface SuperTokenOperation {
    type: SuperTokenOperationType
    data: {
        token: string
        amount: string
    }
}

// SuperToken Call Agreement
type SuperFluidCallAgreementType = 'SUPERFLUID_CALL_AGREEMENT' | 201;

// Constant Flow Agreement
type CFAMethodType =
    'createFlow' |
    'updateFlow' |
    'deleteFlow' |
    'getFlow' |
    'getNetFlow' |
    'getAccountFlowInfo' |
    'getFlowEvents';

interface SuperTokenCFAData {
    agreementType: 'CFA'
    method: CFAMethodType
    arguments: Array<string | (() => any)>
}

// Instant Distribution Agreement
type IDAMethodType =
    'createIndex' |
    'distribute' |
    'updateIndex' |
    'updateSubscription' |
    'approveSubscription' |
    'revokeSubscription' |
    'deleteSubscription' |
    'getSubscription' |
    'claim' |
    'getIndex' |
    'listIndices' |
    'listSubcribers' |
    'listSubscriptions';

interface SuperTokenIDAData {
    agreementType: 'IDA'
    method: IDAMethodType
    arguments: Array<string | number | Promise<Transaction> | (()=>any)>
}

interface SuperFluidCallAgreement {
    type: SuperFluidCallAgreementType
    data: SuperTokenCFAData | SuperTokenIDAData
}

// Call App Action
type CallAppActionType = 'CALL_APP_ACTION' | 202;
interface CallAppAction {
    type: CallAppActionType
    data: {
        superApp: string
        callData: string
    }
}

// SuperFluid Operation
// Combines superfluid ida/cfa type with call app action type
type SuperFluidOperation = SuperFluidCallAgreement | CallAppAction;

export type Agreement = ERC20Operation | SuperTokenOperation | SuperFluidOperation;

export var agreements: Array<Agreement>;

// return set to 'any' for now
export function batchCall({ agreements, calls }: {
    agreements: Array<Agreement>;
    calls: Array<Agreement>;
}): any[][];
