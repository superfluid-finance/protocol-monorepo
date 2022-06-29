import { Divider } from "antd";
import { Address } from "../components";

export default function NFTBillboard({
  message,
  billboardOwner,
  readContracts,
}) {

  return (
    <div>
      {/*
        ⚙️ Here is a UI that displays and sets the message on your NFT Billboard
      */}
      <div style={{ border: "1px solid #cccccc", padding: 16, width: 400, margin: "auto", marginTop: 64 }}>
        <h1>NFT Billboard</h1>
        <h2>Message: <b>{message}</b></h2>
        <Divider />
        <div style={{ margin: 8 }}>
            <h3>Owner:</h3>
            <h4>
                <Address address={billboardOwner} />
            </h4>
        </div>
                
      </div>


      <div style={{ width: 600, margin: "auto", marginTop: 32, paddingBottom: 256 }}>
      
      Billboard Contract Address:
        <Address
          address={readContracts && readContracts.TradeableCashflow ? readContracts.TradeableCashflow.address : null}
          fontSize={16}
        />

      </div>
    </div>
  );
}
