import React from "react";
import { Span, BottomTable, XL } from "./";
import { flowForHumans } from "../utils/utils.js";

const { wad4human } = require("@decentral.ee/web3-helpers");
export function TableOfPlayers({ playerList, winner }) {
  var items = [];
  //console.log("this is playerList");
  //console.log(playerList);
  if (playerList.length > 0) {
    //console.log("playerList definitely >0");
    var i = 0;
    for (const value of playerList) {
      const { address, flowRate, total } = value;
      //console.log("address: ", address, " netFlow: ", flowRate);
      var item = (
        <tr key={i++}>
          <td>{address}</td>
          <td>
            {address === winner ? (
              <Span color={"green"}>+{flowForHumans(flowRate)}</Span>
            ) : (
              <Span color={"red"}>-{flowForHumans(flowRate)}</Span>
            )}
          </td>
          <td>
            {"Won so far: " + wad4human(total)}
            {address === winner && <XL>👑</XL>}
          </td>
        </tr>
      );
      if (address === winner) items.unshift(item);
      else items.push(item);
    }
  }
  return (
    <BottomTable>
      <h3>Active Players</h3>
      <table>
        <thead>
          <tr key={0}>
            <th>{"Address"}</th>
            <th>{"FlowRate"}</th>
            <th>{"Won so far"}</th>
          </tr>
        </thead>
        <tbody>
        {items}
        </tbody>
      </table>
    </BottomTable>
  );
}

export function TableOfWinners({ winnerList }) {
  var items = [];
  console.log(winnerList);
  if (winnerList.length > 0) {
    var i = 0;
    for (const value of winnerList) {
      const { address, duration, total, flowRate } = value;
      var item = (
        <tr key={i++}>
          <td>{address}</td>
          <td>
            {Math.round(duration / 60)}
            {" minutes"}
          </td>
          <td>{flowForHumans(flowRate)}</td>
          <td>
            {wad4human(total)} {"  won"}
          </td>
        </tr>
      );
      items.unshift(item);
    }
  }
  return (
    <BottomTable>
      <h3>Winners</h3>
      <table>
        <thead>
          <tr key={0}>
            <th>{"Address"}</th>
            <th>{"Winning for"}</th>
            <th>{"FlowRate"}</th>
            <th>{"Total Won"}</th>
          </tr>
        </thead>
        <tbody>
        {items}
        </tbody>
      </table>
    </BottomTable>
  );
}
