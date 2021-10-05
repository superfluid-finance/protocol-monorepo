import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { MATIC_HOST_ADDRESS, MATIC_RESOLVER_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function maticHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(
        event,
        Address.fromString(MATIC_HOST_ADDRESS),
        Address.fromString(MATIC_RESOLVER_ADDRESS)
    );
}
