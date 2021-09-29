import { ContractReceipt } from "ethers";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { waitUntilBlockIndexed } from "../helpers/helpers";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { FlowActionType } from "../helpers/constants";
import { ConstantFlowAgreementV1Helper } from "@superfluid-finance/js-sdk/src/ConstantFlowAgreementV1Helper";

/**
 * Create/Update/Delete a flow between a sender and receiver.
 * Also waits for the graph to index and also returns the receipt
 * of the txn and data from the blockchain.
 * @param sf
 * @param cfaV1
 * @param actionType
 * @param superToken
 * @param sender
 * @param receiver
 * @param newFlowRate
 * @returns txnReceipt, flow updatedAt (on-chain), flowRate (current on-chain)
 */
export const modifyFlowAndReturnCreatedFlowData = async (
    sf: Framework,
    cfaV1: ConstantFlowAgreementV1,
    actionType: FlowActionType,
    superToken: string,
    sender: string,
    receiver: string,
    newFlowRate: number
) => {
    const actionToTypeStringMap = new Map([
        [FlowActionType.Create, "Create"],
        [FlowActionType.Update, "Update"],
        [FlowActionType.Delete, "Delete"],
    ]);
    console.log(
        `********************** ${actionToTypeStringMap.get(
            actionType
        )} a flow **********************`
    );
    const sfCFA = sf.cfa as ConstantFlowAgreementV1Helper;
    // any because it the txn.receipt doesn't exist on
    // Transaction
    const txn: any =
        actionType === FlowActionType.Create
            ? await sfCFA.createFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : actionType === FlowActionType.Update
            ? await sfCFA.updateFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : await sfCFA.deleteFlow({
                  superToken,
                  sender,
                  receiver,
                  by: "",
                  userData: "0x",
                  onTransaction: () => {},
              });

    const receipt: ContractReceipt = txn.receipt;

    await waitUntilBlockIndexed(receipt.blockNumber);

    const [updatedAtTimestamp, flowRate] = await cfaV1.getFlow(
        superToken,
        sender,
        receiver
    );
    return {
        receipt,
        updatedAtTimestamp,
        flowRate,
    };
};
