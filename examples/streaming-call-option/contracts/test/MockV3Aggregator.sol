pragma solidity ^0.7.0;

import "@chainlink/contracts/src/v0.7/interfaces/AggregatorV3Interface.sol";


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