import React, { useCallback, useEffect, useState } from "react";

import { Web3Provider } from "@ethersproject/providers";
import Web3 from "web3";
import AnimatedNumber from "animated-number-react";
import {
  Body,
  Button,
  Header,
  BoxContainer,
  Box,
  ShrinkBox,
  Center,
  Span,
  Div100,
} from "./components";
import { web3Modal, logoutOfWeb3Modal } from "./utils/web3Modal";
import { TableOfPlayers, TableOfWinners } from "./components/BottomTables";
import { flowForHumans, showTick } from "./utils/utils";

const TruffleContract = require("@truffle/contract");

const APP_ADDRESS = "0x218eBC19aD9E721e6145eC4bA7Fe37e6A2EEcD1a";
const MINIMUM_GAME_FLOW_RATE = "3858024691358";
const LotterySuperApp = TruffleContract(require("./LotterySuperApp.json"));

const { wad4human } = require("@decentral.ee/web3-helpers");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

function WalletButton({ provider, userAddress, loadWeb3Modal }) {
  return (
    <Button
      onClick={() => {
        if (!provider) {
          loadWeb3Modal();
        } else {
          logoutOfWeb3Modal();
        }
      }}
    >
      {!provider ? (
        "Connect Wallet"
      ) : (
        <>
          <span>"Disconnect Wallet"</span>
          <br />
          <small>{userAddress.slice(0, 10) + "..."}</small>
        </>
      )}
    </Button>
  );
}

let sf;
let dai;
let daix;
let app;

const ZERO_ADDRESS = "0x"+"0".repeat(40);

function App() {
  const [provider, setProvider] = useState();
  const [daiApproved, setDAIapproved] = useState(0);
  const [joinedLottery, setJoinedLottery] = useState();
  const [userAddress, setUserAddress] = useState(ZERO_ADDRESS);
  const [winnerAddress, setWinnerAddress] = useState(ZERO_ADDRESS);
  const [daiBalance, setDaiBalance] = useState(0);
  const [daixBalance, setDaixBalance] = useState(0);
  const [daixBalanceFake, setDaixBalanceFake] = useState(0);
  const [userNetFlow, setUserNetFlow] = useState(0);
  const [playerList, setPlayerList] = useState([]);
  const [winnerLog, setWinnerLog] = useState([]);
  const [lastCheckpoint, setLastCheckpoint] = useState([]);

  async function mintDAI(amount = 100) {
    //mint some dai here!  100 default amount
    await dai.mint(
      userAddress,
      sf.web3.utils.toWei(amount.toString(), "ether"),
      { from: userAddress }
    );
    setDaiBalance(wad4human(await dai.balanceOf.call(userAddress)));
  }

  async function approveDAI() {
    //approve unlimited please
    await dai
      .approve(
        daix.address,
        "115792089237316195423570985008687907853269984665640564039457584007913129639935",
        { from: userAddress }
      )
      .then(async i =>
        setDAIapproved(
          wad4human(await dai.allowance.call(userAddress, daix.address))
        )
      );
  }

  async function joinLottery() {
    setDaiBalance(wad4human(await dai.balanceOf.call(userAddress)));
    setDaixBalance(wad4human(await daix.balanceOf.call(userAddress)));
    let call = [];
    if (daixBalance < 2)
      call = [
        [
          1 + 100, // upgrade 100 daix to play the game
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["uint256"],
            [sf.web3.utils.toWei("100", "ether").toString()]
          )
        ]
      ];
    call.push(...[
        [
          1, // approve the ticket fee
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["address", "uint256"],
            [APP_ADDRESS, sf.web3.utils.toWei("1", "ether").toString()]
          )
        ],
        [
          2 + 200, // callAppAction to participate
          app.address,
          app.contract.methods.participate("0x").encodeABI()
        ],
        [
          1 + 200, // create constant flow (10/mo)
          sf.agreements.cfa.address,
          sf.web3.eth.abi.encodeParameters(
            ["bytes", "bytes"],
            [
              sf.agreements.cfa.contract.methods
                .createFlow(
                  daix.address,
                  app.address,
                  MINIMUM_GAME_FLOW_RATE.toString(),
                  "0x"
                )
                .encodeABI(),
              "0x"
            ]
          )
        ]
    ]);
    console.log("this is the batchcall: ", call);
    await sf.host.batchCall(call, { from: userAddress });
    await checkWinner();
  }

  async function leaveLottery() {
    await sf.host.callAgreement(
      sf.agreements.cfa.address,
      sf.agreements.cfa.contract.methods
        .deleteFlow(daix.address, userAddress, app.address, "0x")
        .encodeABI(),
      "0x",
      { from: userAddress }
    );
    await checkWinner();
  }

  const checkWinner = useCallback(async () => {
    console.log("Checking winner...");
    await app.currentWinner.call().then(async p => {
      console.log("New winner", p.player);
      setWinnerAddress(p.player);
      setLastCheckpoint(Date.now());
      setDaixBalance(wad4human(await daix.balanceOf.call(userAddress)));
      setDaixBalanceFake(wad4human(await daix.balanceOf.call(userAddress)));
    });
  }, [userAddress]);

  /* Open wallet selection modal. */
  const loadWeb3Modal = useCallback(async () => {
    const newProvider = await web3Modal.connect();

    newProvider.on("accountsChanged", accounts => {
      console.log("accountsChanged", accounts);
      setUserAddress(accounts[0]);
      checkWinner();
    });

    sf = new SuperfluidSDK.Framework({
      web3: new Web3(newProvider),
      tokens: ["fDAI"]
    });
    await sf.initialize();

    dai = await sf.contracts.TestToken.at(sf.tokens.fDAI.address);
    daix = sf.tokens.fDAIx;
    LotterySuperApp.setProvider(newProvider);
    app = await LotterySuperApp.at(APP_ADDRESS);

    global.web3 = sf.web3;

    const accounts = await sf.web3.eth.getAccounts();
    setUserAddress(accounts[0]);

    setProvider(new Web3Provider(newProvider));

    setInterval(function() {
      return checkWinner();
    }, 10000);
    checkWinner();
  }, [checkWinner]);

  /* If user has loaded a wallet before, load it automatically. */
  useEffect(() => {
    if (web3Modal.cachedProvider) {
      loadWeb3Modal();
    }
    // ############################ here you do all the data retrieval: please pull all the current players in the lottery and push them using addPlayer({address, netFlow})
  }, [loadWeb3Modal]);
  function increaseBalance(value) {
    //console.log("netflow: ", userNetFlow / 1e18);
    //console.log("daixBalanceFake: ", daixBalanceFake);
    var newBalance = Number(daixBalanceFake) + (Number(userNetFlow) * 5) / 1e18;
    if (
      (userNetFlow < 0 && newBalance < daixBalanceFake) ||
      (userNetFlow > 0 && newBalance > daixBalanceFake)
    )
      setDaixBalanceFake(newBalance);
  }
  function getLatestFlows(flows) {
    return Object.values(
      flows.reduce((acc, i) => {
        acc[i.args.sender + ":" + i.args.receiver] = i;
        return acc;
      }, {})
    ).filter(i => i.args.flowRate.toString() !== "0");
  }
  useEffect(() => {
    console.log("Refresh players list");
    (async () => {
      if (provider && sf.agreements) {
        setDaiBalance(wad4human(await dai.balanceOf.call(userAddress)));
        setDaixBalance(wad4human(await daix.balanceOf.call(userAddress)));
        setDaixBalanceFake(wad4human(await daix.balanceOf.call(userAddress)));
        setDAIapproved(
          wad4human(await dai.allowance.call(userAddress, daix.address))
        );
        const flow = (
          await sf.agreements.cfa.getNetFlow.call(daix.address, userAddress)
        ).toString();
        console.log("user address: ", userAddress);
        console.log("user DAI balance: ", daiBalance);
        console.log("user DAI allowance: ", daiApproved);
        console.log("user DAIx balance: ", daixBalance);
        console.log("winner address: ", winnerAddress);
        console.log("Flow in useEffect() = ", flow);
        setUserNetFlow(flow);
        console.log("userNetFlow:", userNetFlow);
        setJoinedLottery(
          (
            await sf.agreements.cfa.getFlow(
              daix.address,
              userAddress,
              app.address
            )
          ).timestamp > 0
        );
        var winnerFlow = (
          await sf.agreements.cfa.getFlow.call(
            daix.address,
            app.address,
            winnerAddress
          )
        ).flowRate.toString();
        var newList = getLatestFlows(
          await sf.agreements.cfa.getPastEvents("FlowUpdated", {
            fromBlock: 0,
            filter: {
              receiver: app.address
            }
          })
        );
        newList = newList.map(f => {
          var flowRate =
            f.args.sender === winnerAddress
              ? winnerFlow
              : f.args.flowRate.toString();
          //console.log("flowrate in mapping ", flowRate);
          return {
            address: f.args.sender,
            flowRate
          };
        });
        var newWinnerLog = await sf.agreements.cfa.getPastEvents(
          "FlowUpdated",
          {
            fromBlock: 0,
            filter: {
              sender: app.address
            }
          }
        );
        newWinnerLog = newWinnerLog.map(f => {
          return {
            address: f.args.receiver,
            blockNumber: f.blockNumber,
            flowRate: f.args.flowRate.toString()
          };
        });

        var pluckedWinnerLog = [];
        for (var i = 1; i < newWinnerLog.length; i++) {
          if (typeof newWinnerLog[i] !== "undefined") {
            if (newWinnerLog[i].address === newWinnerLog[i - 1].address) {
              var fromTimestamp = (
                await sf.web3.eth.getBlock(newWinnerLog[i - 1].blockNumber)
              ).timestamp;
              var toTimestamp = (
                await sf.web3.eth.getBlock(newWinnerLog[i].blockNumber)
              ).timestamp;
              var flowRate = Math.max(
                newWinnerLog[i].flowRate,
                newWinnerLog[i - 1].flowRate
              );
              pluckedWinnerLog.push({
                address: newWinnerLog[i].address,
                flowRate,
                fromTimestamp,
                toTimestamp,
                duration: toTimestamp - fromTimestamp,
                total: flowRate * (toTimestamp - fromTimestamp)
              });
            }
          }
        }
        for (var player of newList) {
          player.total = 0;
          for (var prize of pluckedWinnerLog) {
            if (
              String(prize.address).toLowerCase() ===
              String(player.address).toLowerCase()
            )
              player.total += prize.total;
          }
        }
        console.log(newList);
        console.log(pluckedWinnerLog);
        setWinnerLog(pluckedWinnerLog);
        setPlayerList(newList);
      }
    })();
  }, [daiApproved, daiBalance, daixBalance, lastCheckpoint, provider, userAddress, userNetFlow, winnerAddress]);

  return (
    <Body>
      <div>
        <Header>
          <Div100>
            <h2>Flow lottery - Built on Superfluid</h2>
          </Div100>
          <WalletButton
            userAddress={userAddress}
            provider={provider}
            loadWeb3Modal={loadWeb3Modal}
          />
        </Header>
        {/*<Image src={logo} alt="react-logo" />*/}
        {/* Remove the "hidden" prop and open the JavaScript console in the browser to see what this function does */}
        <BoxContainer
          winner={
            !joinedLottery
              ? "notPlaying"
              : winnerAddress === userAddress
              ? "winner"
              : "loser"
          }
        >
          <Box>
            <div>
              <p> Your DAI balance: {daiBalance}</p>
              <p>
                {" "}
                Your DAIx balance:
                <AnimatedNumber
                  value={daixBalanceFake}
                  complete={increaseBalance}
                  duration={5000}
                />
              </p>
              <p>
                {" "}
                Your net flow:{" "}
                <Span color={userNetFlow > 0 ? "green" : "red"}>
                  {flowForHumans(userNetFlow)}
                </Span>
              </p>
              {!joinedLottery ? (
                <h1>Join the game!</h1>
              ) : winnerAddress === userAddress ? (
                <h1>You're winning!</h1>
              ) : (
                <h1>You're losing!</h1>
              )}
            </div>
          </Box>
          <Box></Box>
          <ShrinkBox>
            <Button onClick={() => mintDAI()}>
              1. Mint some DAI{" "}
              {showTick(
                (daiBalance >= 2 && daiBalance !== "0") || daixBalance > 2
              )}
            </Button>
            <Button onClick={() => approveDAI()}>
              2. Approve DAI{" "}
              {showTick(Number(daiApproved) > 0 && daiApproved !== "0")}
            </Button>
            <Button onClick={() => joinLottery()} disabled={joinedLottery}>
              3. Join Lottery
            </Button>
            <Button onClick={() => leaveLottery()} disabled={!joinedLottery}>
              4. Leave Lottery
            </Button>
          </ShrinkBox>
        </BoxContainer>
        <Div100>
          <Center>
            <h4>
              New winner every time a players joins or leaves.
              <br />
              Stay in the game for a chance to win!
              <br />
            </h4>
          </Center>
        </Div100>
        <TableOfPlayers playerList={playerList} winner={winnerAddress} />
        <TableOfWinners winnerList={winnerLog} />
      </div>
    </Body>
  );
}

export default App;
