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
import { XDAI_HOST_ADDRESS, XDAI_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleSent,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(XDAI_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(XDAI_RESOLVER_ADDRESS);

export function xdaiHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleSent(event: SentEvent): void {
    handleSent(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function xdaiHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
