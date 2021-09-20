import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { MATIC_HOST_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(MATIC_HOST_ADDRESS);

export function maticHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS);
}

export function maticHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS);
}

export function maticHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS);
}

export function maticHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS);
}
