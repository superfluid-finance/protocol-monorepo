import {
    Approval,
    ApprovalForAll,
    Transfer,
    MetadataUpdate,
} from "../../generated/ConstantInflowNFT/IFlowNFTBase";
import {
    ApprovalEvent,
    ApprovalForAllEvent,
    MetadataUpdateEvent,
    TransferEvent,
} from "../../generated/schema";
import { getOrInitAccount } from "../mappingHelpers";
import { BIG_INT_ONE, createEventID, initializeEventEntity } from "../utils";

export function handleApproval(event: Approval): void {
    const eventId = createEventID("Approval", event);
    const ev = new ApprovalEvent(eventId);
    initializeEventEntity(ev, event, [event.address, event.params.owner, event.params.approved]);
    ev.owner = event.params.owner.toHex();
    ev.to = event.params.approved.toHex();
    ev.tokenId = event.params.tokenId;
    ev.amount = BIG_INT_ONE.neg();
    ev.isNFTApproval = true;

    ev.save();
}

export function handleApprovalForAll(event: ApprovalForAll): void {
    const eventId = createEventID("ApprovalForAll", event);
    const ev = new ApprovalForAllEvent(eventId);
    initializeEventEntity(ev, event, [event.address, event.params.owner, event.params.operator]);
    ev.owner = event.params.owner.toHex();
    ev.operator = event.params.operator.toHex();
    ev.approved = event.params.approved;

    ev.save();
}

export function handleTransfer(event: Transfer): void {
    const eventId = createEventID("Transfer", event);
    const ev = new TransferEvent(eventId);
    initializeEventEntity(ev, event, [
        event.address,
        event.params.from,
        event.params.to,
    ]);
    ev.isNFTTransfer = true;
    ev.from = event.params.from.toHex();
    ev.to = event.params.to.toHex();
    ev.value = event.params.tokenId;
    ev.token = event.address;

    ev.save();

    getOrInitAccount(event.params.to, event.block);
    getOrInitAccount(event.params.from, event.block);
}

export function handleMetadataUpdate(event: MetadataUpdate): void {
    const eventId = createEventID("MetadataUpdate", event);
    const ev = new MetadataUpdateEvent(eventId);
    initializeEventEntity(ev, event, []);
    ev.tokenId = event.params.tokenId;

    ev.save();
}