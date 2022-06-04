// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0;

contract MockV3Aggregator {
    int256 public _latestRoundData;

    event AnswerUpdated(int256 indexed current, uint256 indexed roundId, uint256 timestamp);

    constructor (int256 _initialAnswer)  {
        _latestRoundData = _initialAnswer;
        emit AnswerUpdated(_initialAnswer, 0, block.timestamp);
    }

    function latestRoundData() external view returns (int256, int256, int256, int256, int256) {
        return (0 ,_latestRoundData, 0, 0, 0);
    }
}