import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { MUMBAI_HOST_ADDRESS, MUMBAI_RESOLVER_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function mumbaiHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(
        event,
        Address.fromString(MUMBAI_HOST_ADDRESS),
        Address.fromString(MUMBAI_RESOLVER_ADDRESS)
    );
}
