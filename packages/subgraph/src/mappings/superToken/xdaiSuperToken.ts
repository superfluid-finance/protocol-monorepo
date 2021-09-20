import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { XDAI_HOST_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(XDAI_HOST_ADDRESS);

export function xdaiHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS);
}

export function xdaiHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS);
}

export function xdaiHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS);
}

export function xdaiHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS);
}
