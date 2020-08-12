pragma solidity >=0.6.0;

interface ISuperfluid {

    function isJailed(address app) external view returns(bool);
    function registerSuperApp(uint256 manifest) external;
    function getConfig(address superApp) external view returns(uint256);
    function setAppConnection(address appModule) external;
    function setWhiteList(address module) external;
    function isWhiteListed(address sender, address app) external view returns(bool);
    function callBuildContext(
        address callAddr,
        uint64 gasReservation,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory);
    function callWithContext(
        bytes calldata ctx,
        address callAddr,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory);
}
