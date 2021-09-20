import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { MATIC_HOST_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function goerliHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(event, Address.fromString(MATIC_HOST_ADDRESS));
}
