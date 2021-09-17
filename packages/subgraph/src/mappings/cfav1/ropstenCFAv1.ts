import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { ROPSTEN_HOST_ADDRESS } from "../../utils";
import { HandleStreamUpdated } from "./cfav1Base";

export function goerliHandleStreamUpdated(event: FlowUpdatedEvent): void {
    HandleStreamUpdated(event, Address.fromString(ROPSTEN_HOST_ADDRESS));
}
