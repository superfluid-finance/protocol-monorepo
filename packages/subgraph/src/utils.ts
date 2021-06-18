import {
    BigInt,
    BigDecimal,
    EthereumEvent,
    Address,
    log,
    dataSource,
    DataSourceContext,
} from "@graphprotocol/graph-ts";

import { SuperToken as SuperTokenTemplate } from "../generated/templates";
import {
    Account,
    Flow,
    Token,
    Transaction,
    AccountWithToken,
} from "../generated/schema";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { ISuperfluid as SuperFluid } from "../generated/SuperTokenFactory/ISuperfluid";
import { ISuperTokenFactory as SuperTokenFactory } from "../generated/SuperTokenFactory/ISuperTokenFactory";

export function createEventID(event: EthereumEvent): string {
    return event.block.number
        .toString()
        .concat("-")
        .concat(event.logIndex.toString());
}

function createFlowID(owner: string, recipient: string, token: string): string {
    return owner
        .concat("-")
        .concat(recipient)
        .concat("-")
        .concat(token);
}

export function logTransaction(event: EthereumEvent): Transaction {
    let tx = new Transaction(event.transaction.hash.toHex());
    tx.timestamp = event.block.timestamp;
    tx.blockNumber = event.block.number;
    tx.save();
    return tx as Transaction;
}

export function toDai(value: BigInt): BigDecimal {
    return value.divDecimal(BigDecimal.fromString("1000000000000000000")); // 18 decimal
}

export function fetchAccount(id: string): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
    }
    return account as Account;
}

export function fetchToken(address: string): Token {
    let token = Token.load(address);
    if (token == null) {
        let tokenContract = SuperToken.bind(Address.fromString(address));
        let underlyingAddress = tokenContract.getUnderlyingToken();
        let name = tokenContract.name();
        let symbol = tokenContract.symbol();
        token = new Token(address);
        token.underlyingAddress = underlyingAddress;
        token.name = name;
        token.symbol = symbol;
        // Create a dynamic data source instance
        // https://thegraph.com/docs/define-a-subgraph#instantiating-a-data-source-template
        // TODO: pull the cfa address directly from the subgraph.yaml
        // Waiting for a response: https://discord.com/channels/438038660412342282/438070183794573313/855252832353386506
        // and remove all these dumb web3 calls
        // Theoretically this mapping should be initiated from the superTokenFactory contract
        // Cross your finders dataSource.address() is indeed the factory address
        let superTokenFactory = SuperTokenFactory.bind(dataSource.address());
        let hostAddress = superTokenFactory.getHost();
        let host = SuperFluid.bind(hostAddress);
        let cfaAddress = host.getAgreementClass(
            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1".toHex()
        );
        let context = new DataSourceContext();
        context.setString("cfaAddress", cfaAddress);
        SuperTokenTemplate.createWithContext(
            Address.fromString(address),
            context
        );
    }
    return token as Token;
}

export function fetchAccountWithToken(
    accountId: string,
    tokenId: string
): AccountWithToken {
    let id = accountId.concat("-").concat(tokenId);
    let accountWithToken = AccountWithToken.load(id);
    if (accountWithToken == null) {
        let account = fetchAccount(accountId); // Ensure these exist
        account.save();
        let token = fetchToken(tokenId);
        token.save();
        accountWithToken = new AccountWithToken(id);
        accountWithToken.balance = BigDecimal.fromString("0");
        accountWithToken.account = accountId;
        accountWithToken.token = tokenId;
    }
    return accountWithToken as AccountWithToken;
}

export function fetchFlow(
    ownerAddress: string,
    recipientAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Flow {
    let id = createFlowID(ownerAddress, recipientAddress, tokenAddress);
    let flow = Flow.load(id);
    if (flow == null) {
        flow = new Flow(id);
        flow.sum = BigDecimal.fromString("0");
        flow.flowRate = BigInt.fromI32(0);
        let token = fetchToken(tokenAddress);
        flow.token = token.id;
        flow.owner = ownerAddress;
        flow.lastUpdate = timestamp;
        flow.recipient = recipientAddress;

        // Create accounts and tokens if they do not exist
        let ownerAccount = fetchAccount(ownerAddress);
        let recipientAccount = fetchAccount(recipientAddress);
        ownerAccount.save();
        recipientAccount.save();
        token.save();
    }
    return flow as Flow;
}

export function updateBalance(
    accountId: string,
    tokenId: string,
    amount: BigDecimal,
    increase: bool,
    timestamp: BigInt
): AccountWithToken {
    let accountWithToken = fetchAccountWithToken(accountId, tokenId);
    let balance = accountWithToken.balance;
    if (increase) {
        accountWithToken.balance = balance + amount;
    } else {
        accountWithToken.balance = balance - amount;
    }

    // cfa = dataSource.ConstantFlowAgreementV1
    // let tokenContract = SuperToken.bind(Address.fromString(tokenId));
    // let realTimeBalance = tokenContract.realTimeBalanceOf(accountId, timestamp)
    //     .dynamicBalance;
    // accountWithToken.realTimeBalance = realTimeBalance;
    // accountWithToken.lastUpdate = timestamp;
    accountWithToken.save();
    return accountWithToken as AccountWithToken;
}
