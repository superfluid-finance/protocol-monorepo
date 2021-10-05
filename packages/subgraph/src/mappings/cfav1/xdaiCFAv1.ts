import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { XDAI_HOST_ADDRESS, XDAI_RESOLVER_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function xdaiHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(
        event,
        Address.fromString(XDAI_HOST_ADDRESS),
        Address.fromString(XDAI_RESOLVER_ADDRESS)
    );
}
