import { WalletOutlined } from "@ant-design/icons";
import { Button, Modal, Spin, Tooltip, Typography } from "antd";
import { ethers } from "ethers";
import QR from "qrcode.react";
import React, { useState, useEffect } from "react";
import Address from "./Address";

const { Text, Paragraph } = Typography;

export default function Wallet(props) {
  const [signerAddress, setSignerAddress] = useState();
  useEffect(() => {
    async function getAddress() {
      if (props.signer) {
        const newAddress = await props.signer.getAddress();
        setSignerAddress(newAddress);
      }
    }
    getAddress();
  }, [props.signer]);

  const selectedAddress = props.address || signerAddress;

  const [open, setOpen] = useState();
  const [qr, setQr] = useState();
  // const [toAddress, setToAddress] = useState();
  const [pk, setPK] = useState();

  const providerSend = props.provider ? (
    <Tooltip title="Wallet">
      <WalletOutlined
        onClick={() => {
          setOpen(!open);
        }}
        rotate={-90}
        style={{
          padding: 7,
          color: props.color ? props.color : "",
          cursor: "pointer",
          fontSize: 28,
          verticalAlign: "middle",
        }}
      />
    </Tooltip>
  ) : (
    ""
  );

  let display;
  if (qr) {
    display = (
      <div>
        <div>
          <Text copyable>{selectedAddress}</Text>
        </div>
        <QR
          value={selectedAddress}
          size="450"
          level="H"
          includeMargin
          renderAs="svg"
          imageSettings={{ excavate: false }}
        />
      </div>
    );


  } else if (pk) {
    const pk = localStorage.getItem("metaPrivateKey");
    const wallet = new ethers.Wallet(pk);

    if (wallet.address !== selectedAddress) {
      display = (
        <div>
          <b>*injected account*, private key unknown</b>
        </div>
      );
    } else {
      const extraPkDisplayAdded = {};
      const extraPkDisplay = [];
      extraPkDisplayAdded[wallet.address] = true;
      extraPkDisplay.push(
        <div style={{ fontSize: 16, padding: 2, backgroundStyle: "#89e789" }}>
          <a href={"/pk#" + pk}>
            <Address minimized address={wallet.address} /> {wallet.address.substr(0, 6)}
          </a>
        </div>,
      );
      for (const key in localStorage) {
        if (key.indexOf("metaPrivateKey_backup") >= 0) {
          console.log(key);
          const pastpk = localStorage.getItem(key);
          const pastwallet = new ethers.Wallet(pastpk);
          if (!extraPkDisplayAdded[pastwallet.address] /* && selectedAddress!=pastwallet.address */) {
            extraPkDisplayAdded[pastwallet.address] = true;
            extraPkDisplay.push(
              <div style={{ fontSize: 16 }}>
                <a href={"/pk#" + pastpk}>
                  <Address minimized address={pastwallet.address} />{" "}
                  {pastwallet.address.substr(0, 6)}
                </a>
              </div>,
            );
          }
        }
      }

      display = (
        <div>
          <b>Private Key:</b>

          <div>
            <Text copyable>{pk}</Text>
          </div>

          <hr />

          <i>
            Point your camera phone at qr code to open in
            <a target="_blank" href={"https://xdai.io/" + pk} rel="noopener noreferrer">
              burner wallet
            </a>
            :
          </i>
          <QR
            value={"https://xdai.io/" + pk}
            size="450"
            level="H"
            includeMargin
            renderAs="svg"
            imageSettings={{ excavate: false }}
          />

          <Paragraph style={{ fontSize: "16" }} copyable>
            {"https://xdai.io/" + pk}
          </Paragraph>

          {extraPkDisplay ? (
            <div>
              <h3>Known Private Keys:</h3>
              {extraPkDisplay}
              <Button
                onClick={() => {
                  const currentPrivateKey = window.localStorage.getItem("metaPrivateKey");
                  if (currentPrivateKey) {
                    window.localStorage.setItem("metaPrivateKey_backup" + Date.now(), currentPrivateKey);
                  }
                  const randomWallet = ethers.Wallet.createRandom();
                  const privateKey = randomWallet._signingKey().privateKey;
                  window.localStorage.setItem("metaPrivateKey", privateKey);
                  window.location.reload();
                }}
              >
                Generate
              </Button>
            </div>
          ) : (
            ""
          )}
        </div>
      );
    }
  }

  return (
    <span>
      {providerSend}
      <Modal
        visible={open}
        title={
          <div>
            {selectedAddress ? <Address address={selectedAddress} /> : <Spin />}
            <div style={{ float: "right", paddingRight: 25 }}>
            </div>
          </div>
        }
        onOk={() => {
          setQr();
          setPK();
          setOpen(!open);
        }}
        onCancel={() => {
          setQr();
          setPK();
          setOpen(!open);
        }}
      >
        {display}
      </Modal>
    </span>
  );
}
