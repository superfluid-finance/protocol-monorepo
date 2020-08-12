pragma solidity >=0.6.0;

interface ISuperfluid {

    function isJailed(address app) external view returns(bool);
    function registerSuperApp(uint256 manifest) external;
    function getManifest(address superApp) external view returns(uint256);
    function setAppConnection(address appModule) external;
    function setWhiteList(address module) external;
    function isWhiteListed(address sender, address app) external view returns(bool);
    function callBuildContext(
        address appAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes32 id
    )
        external
        returns(bool);
    function callWithContext(
        bytes calldata ctx
    )
        external
        returns(bool);
}
