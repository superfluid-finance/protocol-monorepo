pragma solidity >=0.6.0;

interface ISuperfluid {

    function isJailed(address app) external view returns(bool);
    function setAppConnection(address appModule) external;
    function setWhiteList(address module) external;
    function isWhiteListed(address sender, address app) external view returns(bool);
    function callWithContext(
        address appAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes32 id
    )
        external
        returns(bool);
}
