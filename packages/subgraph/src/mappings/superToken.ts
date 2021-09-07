import {
    TokenUpgraded as TokenUpgradedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    Transfer as TransferEvent,
	AgreementLiquidatedBy as AgreementLiquidatedByEvent,
} from "../../generated/templates/SuperToken/ISuperToken";
import {
    TokenUpgraded,
    TokenDowngraded,
    Transfer,
	AgreementLiquidatedBy,
} from "../../generated/schema";
import {
    createAndReturnTxn,
    createEventID,
    fetchAccount,
    updateBalance,
} from "../utils";

export function handleAgreementLiquidatedBy(event: AgreementLiquidatedByEvent): void {
	let ev = new AgreementLiquidatedBy(createEventID(event));
	ev.transaction = createAndReturnTxn(event).id;
	ev.token = event.address.toHex();
	ev.liquidatorAccount = event.params.liquidatorAccount;
	ev.agreementClass = event.params.agreementClass;
	ev.agreementId = event.params.id;
	ev.penaltyAccount = event.params.penaltyAccount;
	ev.bondAccount = event.params.bondAccount;
	ev.rewardAmount = event.params.rewardAmount;
	ev.bailoutAmount = event.params.bailoutAmount;
	ev.save();

	// TODO: do we update the balances of the liquidator, penalty and bond account's here? probably yes
}

export function handleTokenUpgraded(event: TokenUpgradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();

    let ev = new TokenUpgraded(createEventID(event));
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    ev.account = event.params.account;
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenId;
    ev.amount = amount;
    ev.save();

    updateBalance(account.id, tokenId);
}

export function handleTokenDowngraded(event: TokenDowngradedEvent): void {
    let account = fetchAccount(event.params.account.toHex());
    account.save();

    let ev = new TokenDowngraded(createEventID(event));
    let amount = event.params.amount;
    let tokenId = event.address.toHex();

    ev.account = event.params.account;
    ev.transaction = createAndReturnTxn(event).id;
    ev.token = tokenId;
    ev.amount = amount;
    ev.save();

    updateBalance(account.id, tokenId);
}

export function handleTransfer(event: TransferEvent): void {
    let fromAccount = fetchAccount(event.params.from.toHex());
    let toAccount = fetchAccount(event.params.to.toHex());
    fromAccount.save();
    toAccount.save();

    let ev = new Transfer(createEventID(event));
    let value = event.params.value;
    let tokenId = event.address.toHex();

    ev.transaction = createAndReturnTxn(event).id;
    ev.from = event.params.from;
    ev.to = event.params.to;
    ev.value = value;
    ev.token = tokenId;
    ev.save();

    updateBalance(toAccount.id, tokenId);
    updateBalance(fromAccount.id, tokenId);
}
