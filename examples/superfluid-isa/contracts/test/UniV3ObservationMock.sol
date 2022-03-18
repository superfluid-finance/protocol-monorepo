pragma solidity ^0.8.0;

contract UniV3ObservationMock {

    //To Do
    //need to use ticks to get the correct price...
    int160 currentPrice;
    address tokenAddress;
    mapping (address => int160) tokenPrices;

    constructor(int160 _initialPrice, address _tokenAddress) {
        currentPrice = _initialPrice;
        tokenAddress = _tokenAddress;
        tokenPrices[_tokenAddress] = currentPrice;
    } 

    function getCurrentPrice(address token) external view returns (int96) {
        return int96(tokenPrices[token]);
    }
}