import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    Burned as BurnedEvent,
    Minted as MintedEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { ROPSTEN_HOST_ADDRESS, ROPSTEN_RESOLVER_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleBurned,
    handleMinted,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(ROPSTEN_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(ROPSTEN_RESOLVER_ADDRESS);

export function ropstenHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleTokenDowngraded(
    event: TokenDowngradedEvent
): void {
    handleTokenDowngraded(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleBurned(event: BurnedEvent): void {
    handleBurned(event);
}

export function ropstenHandleMinted(event: MintedEvent): void {
    handleMinted(event);
}
