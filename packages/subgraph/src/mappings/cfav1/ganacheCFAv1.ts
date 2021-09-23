import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { GANACHE_HOST_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function ganacheHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(event, Address.fromString(GANACHE_HOST_ADDRESS));
}
