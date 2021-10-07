import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    Burned as BurnedEvent,
    Minted as MintedEvent,
    Sent as SentEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { KOVAN_HOST_ADDRESS, KOVAN_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleSent,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(KOVAN_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(KOVAN_RESOLVER_ADDRESS);

export function kovanHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleSent(event: SentEvent): void {
    handleSent(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function kovanHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
