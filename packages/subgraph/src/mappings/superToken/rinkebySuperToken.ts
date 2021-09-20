import { Address } from "@graphprotocol/graph-ts";
import {
    AgreementLiquidatedBy as AgreementLiquidatedByEvent,
    TokenDowngraded as TokenDowngradedEvent,
    TokenUpgraded as TokenUpgradedEvent,
    Transfer as TransferEvent,
} from "../../../generated/templates/SuperToken/ISuperToken";
import { RINKEBY_HOST_ADDRESS } from "../../utils";
import {
    handleAgreementLiquidatedBy,
    handleTokenDowngraded,
    handleTokenUpgraded,
    handleTransfer,
} from "./superTokenBase";

let HOST_ADDRESS = Address.fromString(RINKEBY_HOST_ADDRESS);

export function rinkebyHandleAgreementLiquidatedBy(
    event: AgreementLiquidatedByEvent
): void {
    handleAgreementLiquidatedBy(event, HOST_ADDRESS);
}

export function rinkebyHandleTokenUpgraded(event: TokenUpgradedEvent): void {
    handleTokenUpgraded(event, HOST_ADDRESS);
}

export function rinkebyHandleTokenDowngraded(event: TokenDowngradedEvent): void {
    handleTokenDowngraded(event, HOST_ADDRESS);
}

export function rinkebyHandleTransfer(event: TransferEvent): void {
    handleTransfer(event, HOST_ADDRESS);
}
