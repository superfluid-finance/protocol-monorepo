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
import { GOERLI_HOST_ADDRESS, GOERLI_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleSent,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(GOERLI_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(GOERLI_RESOLVER_ADDRESS);

export function goerliHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleSent(event: SentEvent): void {
    handleSent(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function goerliHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
