pragma solidity >=0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol"; 

import { AggregatorV3Interface } from "@chainlink/contracts/src/v0.8/interfaces/AggregatorV3Interface.sol";

// contract UniV3Observation {

//     //To Do
//     //need to use ticks to get the correct price...
//     // int160 currentPrice;
//     // address tokenAddress;
//     mapping (ISuperToken => uint) tokenPrices;

//     constructor(uint _initialPrice, ISuperToken _token) {
//         // currentPrice = _initialPrice;
//         // tokenAddress = _tokenAddress;
//         tokenPrices[_token] = _initialPrice;
//     } 

//     function setPrice(uint currentPrice, ISuperToken _token) public {
//         // currentPrice = _initialPrice;
//         // tokenAddress = _tokenAddress;
//         tokenPrices[_token] = currentPrice;
//     }

//     function getCurrentPrice(ISuperToken token) external view returns (uint) {
//         return tokenPrices[token];
//     }
// }


contract MockV3Aggregator {
    int256 public _latestRoundData;

    event AnswerUpdated(int256 indexed current, uint256 indexed roundId, uint256 timestamp);

    constructor (int256 _initialAnswer) public  {
        _latestRoundData = _initialAnswer;
        emit AnswerUpdated(_initialAnswer, 0, block.timestamp);
    }

    function latestRoundData() external view returns (int256, int256, int256, int256, int256) {
        return (0 ,_latestRoundData, 0, 0, 0);
    }
}