import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { ROPSTEN_HOST_ADDRESS, ROPSTEN_RESOLVER_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function ropstenHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(
        event,
        Address.fromString(ROPSTEN_HOST_ADDRESS),
        Address.fromString(ROPSTEN_RESOLVER_ADDRESS)
    );
}
