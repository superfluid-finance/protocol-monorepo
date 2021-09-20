import { Address } from "@graphprotocol/graph-ts";
import { FlowUpdated as FlowUpdatedEvent } from "../../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { RINKEBY_HOST_ADDRESS } from "../../utils";
import { handleStreamUpdated } from "./cfav1Base";

export function rinkebyHandleStreamUpdated(event: FlowUpdatedEvent): void {
    handleStreamUpdated(event, Address.fromString(RINKEBY_HOST_ADDRESS));
}
