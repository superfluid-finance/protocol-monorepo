import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    Burned as BurnedEvent,
    Minted as MintedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { GANACHE_HOST_ADDRESS, GANACHE_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(GANACHE_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(GANACHE_RESOLVER_ADDRESS);

export function ganacheHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ganacheHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ganacheHandleTokenDowngraded(
    event: TokenDowngradedEvent
): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ganacheHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ganacheHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function ganacheHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
