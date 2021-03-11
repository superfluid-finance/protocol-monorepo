import {
  BigDecimal,
  BigInt,
  EthereumEvent,
  log
} from "@graphprotocol/graph-ts";

import {
  Superfluid,
} from "../generated/Superfluid/Superfluid";

import {
  ConstantFlowAgreementV1,
  FlowUpdated as FlowUpdatedEvent,
} from "../generated/Superfluid/ConstantFlowAgreementV1";

import {
  Transaction,
  Account,
  Flow,
  Token,
  FlowUpdated
} from "../generated/schema";

import {
  createEventID,
  fetchFlow,
  fetchToken,
  fetchAccount,
  logTransaction,
  toDai
} from "./utils";

export function handleFlowUpdated(event: FlowUpdatedEvent): void {
  let ownerAddress = event.params.sender.toHex()
  let recipientAddress event.params.receiver.toHex()
  let tokenAddress = event.params.token.toHex()
  let flowRate = event.params.token.flowRate.toHex()

  let flow = fetchFlow(ownerAddress, recipientAddress, tokenAddress)
  let oldFlowRate = flow.flowRate

  flow.flowRate = flowRate
  // TODO: Update flow sum
  flow.save()

  let ev = new FlowUpdated(createEventID(event));
  ev.transaction = logTransaction(event).id;
  ev.owner = ownerAddress;
  ev.recipient = recipientAddress;
  ev.oldFlowRate = oldFlowRate;
  ev.flowRate = flowRate
  // TODO: Replace sum with correct value
  ev.sum = BigDecimal.fromString("0");
  ev.save();
}
//
// export function handleHatCreated(event: HatCreatedEvent): void {
//   let hat = new Hat(event.params.hatID.toString());
//   hat.save();
//
//   let hatstats = RToken.bind(event.address).getHatByID(event.params.hatID);
//   let hataccounts = hatstats.value0;
//   let hatportions = hatstats.value1;
//
//   for (let i = 0; i < hataccounts.length; ++i) {
//     let account = fetchAccount(hataccounts[i].toHex());
//     account.save();
//
//     let hatmembership = new HatMembership(
//       hat.id.concat("-").concat(i.toString())
//     );
//     hatmembership.hat = hat.id;
//     hatmembership.account = account.id;
//     hatmembership.portion = hatportions[i];
//     hatmembership.save();
//   }
// }
//
// export function handleInterestPaid(event: InterestPaidEvent): void {
//   // balance is updated by the transfer event
//   let ev = new InterestPaid(createEventID(event));
//   let value = toDai(event.params.amount);
//   ev.transaction = logTransaction(event).id;
//   ev.account = event.params.recipient.toHex();
//   ev.value = value;
//   ev.save();
//
//   let recipientAccount = fetchAccount(event.params.recipient.toHex());
//   let loans = recipientAccount.loansReceived;
//
//   let rToken = RToken.bind(event.address);
//   let savingAssetConversionRate = rToken.savingAssetConversionRate();
//   let iasAddress = rToken.getCurrentAllocationStrategy();
//   let ias = IAllocationStrategy.bind(iasAddress);
//   let exchangeRateStored = ias.exchangeRateStored();
//
//   let sUnredeemed: BigDecimal[];
//   // Get total unredeemed interest
//   let sUnredeemedSum = BigDecimal.fromString("0");
//   for (let i = 0; i < loans.length; ++i) {
//     let loan = Loan.load(loans[i]);
//     let loanInS =
//       (loan.amount * toDai(savingAssetConversionRate)) /
//       toDai(exchangeRateStored);
//     sUnredeemed[i] = loan.sInternal - loanInS;
//     sUnredeemedSum = sUnredeemedSum + sUnredeemed[i];
//   }
//
//   let redeemedAmountInS =
//     (value * toDai(savingAssetConversionRate)) / toDai(exchangeRateStored);
//   if (redeemedAmountInS > sUnredeemedSum) {
//     // TODO: determine if this is an issue
//     let diff = redeemedAmountInS - sUnredeemedSum;
//     log.error(
//       "Redeemed amount is greater than undredeemd S for all loans by {}",
//       [diff.toString()]
//     );
//   }
//   // Reduce each sInternal by redeemed amount * unredeemed interest for loan / total unredeemed interest
//   for (let i = 0; i < loans.length; ++i) {
//     let loan = Loan.load(loans[i]);
//     let loanContributionInS =
//       (sUnredeemed[i] * redeemedAmountInS) / sUnredeemedSum;
//     loan.sInternal = loan.sInternal - loanContributionInS;
//     let loanContribution =
//       (loanContributionInS * toDai(exchangeRateStored)) /
//       toDai(savingAssetConversionRate);
//     // Add the amount earned to interestRedeemed
//     loan.interestRedeemed = loan.interestRedeemed + loanContribution;
//     loan.save();
//   }
// }
//
// export function handleLoansTransferred(event: LoansTransferredEvent): void {
//   let ownerAccount = fetchAccount(event.params.owner.toHex());
//
//   let recipientAccount = fetchAccount(event.params.recipient.toHex());
//   recipientAccount.save();
//
//   let loan = fetchLoan(ownerAccount.id, recipientAccount.id);
//
//   let sign = event.params.isDistribution
//     ? BigDecimal.fromString("1")
//     : BigDecimal.fromString("-1");
//   let delta = sign * toDai(event.params.redeemableAmount);
//   let deltaInS = sign * toDai(event.params.internalSavingsAmount);
//
//   loan.sInternal = loan.sInternal + deltaInS;
//   loan.amount = loan.amount + delta;
//   loan.hat = event.params.hatId.toString();
//
//   let ev = new LoanTransferred(createEventID(event));
//   ev.transaction = logTransaction(event).id;
//   ev.loan = loan.id;
//   ev.value = delta;
//   ev.save();
//
//   loan.save();
// }
//
// export function handleTransfer(event: TransferEvent): void {
//   let fromAccount = fetchAccount(event.params.from.toHex());
//   fromAccount.balance = toDai(
//     RToken.bind(event.address).balanceOf(event.params.from)
//   );
//   fromAccount.save();
//
//   let toAccount = fetchAccount(event.params.to.toHex());
//   toAccount.balance = toDai(
//     RToken.bind(event.address).balanceOf(event.params.to)
//   );
//   toAccount.save();
//
//   let ev = new Transfer(createEventID(event));
//   ev.transaction = logTransaction(event).id;
//   ev.from = event.params.from.toHex();
//   ev.to = event.params.to.toHex();
//   ev.value = toDai(event.params.value);
//   ev.save();
// }
//
// export function handleAllocationStrategyChanged(
//   event: AllocationStrategyChangedEvent
// ): void {}
// export function handleApproval(event: ApprovalEvent): void {}
// export function handleCodeUpdated(event: CodeUpdatedEvent): void {}
// export function handleOwnershipTransferred(
//   event: OwnershipTransferredEvent
// ): void {}
