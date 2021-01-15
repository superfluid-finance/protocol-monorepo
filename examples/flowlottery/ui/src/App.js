import React, { useCallback, useEffect, useState } from "react";

import { Web3Provider } from "@ethersproject/providers";
import { useQuery } from "@apollo/react-hooks";
import AnimatedNumber from "animated-number-react";
import {
  Body,
  Button,
  Header,
  BoxContainer,
  Box,
  ShrinkBox,
  Left,
  Center,
  Right,
  Span,
  BottomTable,
  Div100,
  XL
} from "./components";
import { web3Modal, logoutOfWeb3Modal } from "./utils/web3Modal";
import { TableOfPlayers, TableOfWinners } from "./components/BottomTables";
import { flowForHumans, showTick } from "./utils/utils";

import GET_TRANSFERS from "./graphql/subgraph";
const TruffleContract = require("@truffle/contract");

const APP_ADDRESS = "0x3bf1960Eb675f087208710B7Ec21FB5508576748";
const MINIMUM_GAME_FLOW_RATE = "3858024691358";
const LotterySuperApp = TruffleContract(require("./LotterySuperApp.json"));

const { wad4human } = require("@decentral.ee/web3-helpers");

const SuperfluidSDK = require("@superfluid-finance/ethereum-contracts");

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

function App() {
  const { loading, error, data } = useQuery(GET_TRANSFERS);
  const [provider, setProvider] = useState();
  const [daiApproved, setDAIapproved] = useState(0);
  const [joinedLottery, setJoinedLottery] = useState();
  const [userAddress, setUserAddress] = useState("");
  const [winnerAddress, setWinnerAddress] = useState("");
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
    var call;
    if (daixBalance < 2)
      call = [
        [
          2, // upgrade 100 daix to play the game
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["uint256"],
            [sf.web3.utils.toWei("100", "ether").toString()]
          )
        ],
        [
          0, // approve the ticket fee
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["address", "uint256"],
            [APP_ADDRESS, sf.web3.utils.toWei("1", "ether").toString()]
          )
        ],
        [
          5, // callAppAction to participate
          app.address,
          app.contract.methods.participate("0x").encodeABI()
        ],
        [
          4, // create constant flow (10/mo)
          sf.agreements.cfa.address,
          sf.agreements.cfa.contract.methods
            .createFlow(
              daix.address,
              app.address,
              MINIMUM_GAME_FLOW_RATE.toString(),
              "0x"
            )
            .encodeABI()
        ]
      ];
    else
      call = [
        [
          0, // approve the ticket fee
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["address", "uint256"],
            [APP_ADDRESS, sf.web3.utils.toWei("1", "ether").toString()]
          )
        ],
        [
          5, // callAppAction to participate
          app.address,
          app.contract.methods.participate("0x").encodeABI()
        ],
        [
          4, // create constant flow (10/mo)
          sf.agreements.cfa.address,
          sf.agreements.cfa.contract.methods
            .createFlow(
              daix.address,
              app.address,
              MINIMUM_GAME_FLOW_RATE.toString(),
              "0x"
            )
            .encodeABI()
        ]
      ];
    console.log("this is the batchcall: ", call);
    await sf.host.batchCall(call, { from: userAddress });
    await checkWinner();

    await sf.host.batchCall(
      [
        [
          2, // upgrade 100 daix to play the game
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["uint256"],
            [sf.web3.utils.toWei("100", "ether").toString()]
          )
        ],
        [
          0, // approve the ticket fee
          daix.address,
          sf.web3.eth.abi.encodeParameters(
            ["address", "uint256"],
            [APP_ADDRESS, sf.web3.utils.toWei("1", "ether").toString()]
          )
        ],
        [
          5, // callAppAction to participate
          app.address,
          app.contract.methods.participate("0x").encodeABI()
        ],
        [
          4, // create constant flow (10/mo)
          sf.agreements.cfa.address,
          sf.agreements.cfa.contract.methods
            .createFlow(
              daix.address,
              app.address,
              MINIMUM_GAME_FLOW_RATE.toString(),
              "0x"
            )
            .encodeABI()
        ]
      ],
      { from: userAddress }
    );
  }

  async function leaveLottery() {
    await sf.host.callAgreement(
      sf.agreements.cfa.address,
      sf.agreements.cfa.contract.methods
        .deleteFlow(daix.address, userAddress, app.address, "0x")
        .encodeABI(),
      { from: userAddress }
    );
    await checkWinner();
  }

  const checkWinner = async () => {
    console.log("Checking winner...");
    await app.currentWinner.call().then(async p => {
      console.log("New winner", p.player);
      setWinnerAddress(p.player);
      setLastCheckpoint(Date.now());
      setDaixBalance(wad4human(await daix.balanceOf.call(userAddress)));
      setDaixBalanceFake(wad4human(await daix.balanceOf.call(userAddress)));
    });
  };

  /* Open wallet selection modal. */
  const loadWeb3Modal = useCallback(async () => {
    const newProvider = await web3Modal.connect();

    newProvider.on("accountsChanged", accounts => {
      console.log("accountsChanged", accounts);
      setUserAddress(accounts[0]);
      checkWinner();
    });

    sf = new SuperfluidSDK.Framework({
      chainId: 5,
      version: "0.1.2-preview-20201014",
      web3Provider: newProvider
    });
    await sf.initialize();

    const daiAddress = await sf.resolver.get("tokens.fDAI");
    dai = await sf.contracts.TestToken.at(daiAddress);
    const daixWrapper = await sf.getERC20Wrapper(dai);
    daix = await sf.contracts.ISuperToken.at(daixWrapper.wrapperAddress);
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
  }, []);

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
    ).filter(i => i.args.flowRate.toString() != "0");
  }
  useEffect(() => {
    console.log("Refresh players list");
    (async () => {
      if (provider) {
        setDaiBalance(wad4human(await dai.balanceOf.call(userAddress)));
        setDaixBalance(wad4human(await daix.balanceOf.call(userAddress)));
        setDaixBalanceFake(wad4human(await daix.balanceOf.call(userAddress)));
        setDAIapproved(
          wad4human(await dai.allowance.call(userAddress, daix.address))
        );
        const flow = (await sf.agreements.cfa.getNetFlow.call(
          daix.address,
          userAddress
        )).toString();
        console.log("user address: ", userAddress);
        console.log("user DAI balance: ", daiBalance);
        console.log("user DAI allowance: ", daiApproved);
        console.log("user DAIx balance: ", daixBalance);
        console.log("winner address: ", winnerAddress);
        console.log("Flow in useEffect() = ", flow);
        setUserNetFlow(flow);
        console.log("userNetFlow:", userNetFlow);
        setJoinedLottery(
          (await sf.agreements.cfa.getFlow(
            daix.address,
            userAddress,
            app.address
          )).timestamp > 0
        );
        var winnerFlow = (await sf.agreements.cfa.getFlow.call(
          daix.address,
          app.address,
          winnerAddress
        )).flowRate.toString();
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
              var fromTimestamp = (await sf.web3.eth.getBlock(
                newWinnerLog[i - 1].blockNumber
              )).timestamp;
              var toTimestamp = (await sf.web3.eth.getBlock(
                newWinnerLog[i].blockNumber
              )).timestamp;
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
  }, [lastCheckpoint, provider, userAddress, userNetFlow, winnerAddress]);

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
