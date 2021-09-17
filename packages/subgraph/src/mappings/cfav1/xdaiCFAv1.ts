import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { XDAI_HOST_ADDRESS } from "../../utils";
import { HandleStreamUpdated } from "./cfav1Base";

export function goerliHandleStreamUpdated(event: FlowUpdatedEvent) {
    HandleStreamUpdated(event, Address.fromString(XDAI_HOST_ADDRESS));
}
