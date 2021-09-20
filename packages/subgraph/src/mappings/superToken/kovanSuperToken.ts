import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { KOVAN_HOST_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(KOVAN_HOST_ADDRESS);

export function kovanHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS);
}

export function kovanHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS);
}

export function kovanHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS);
}

export function kovanHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS);
}
