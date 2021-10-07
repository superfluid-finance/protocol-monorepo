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
import { RINKEBY_HOST_ADDRESS, RINKEBY_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleSent,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(RINKEBY_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(RINKEBY_RESOLVER_ADDRESS);

export function rinkebyHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleSent(event: SentEvent): void {
    handleSent(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleTokenDowngraded(
    event: TokenDowngradedEvent
): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function rinkebyHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
