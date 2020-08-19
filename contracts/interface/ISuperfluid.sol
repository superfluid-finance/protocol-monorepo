// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;
pragma experimental ABIEncoderV2;

interface ISuperfluid {

    struct AppManifest {
        uint256 configWord;
    }

    /**
     * @notice Message sender declares it as a super app.
     * @param configWord The super app manifest configuration
     */
    function registerApp(uint256 configWord) external;
    /**
     * @notice Get the manifest of the super app.
     * @param app Super app address
     */
    function getAppManifest(
        address app
    )
        external
        view
        returns (
            bool exist,
            uint256 configWord
        );

    function isAppJailed(address app) external view returns (bool);

    /**
     * @notice White-list the target app for app composition for the source app `msg.sender`
     * @param targetApp The taget super app address
     */
    function allowCompositeApp(address targetApp) external;

    function isCompositeAppAllowed(address app, address targetApp) external view returns (bool);

    function callAppBefore(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory newCtx, bytes memory cbdata);

    function callAppAfter(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory newCtx);

    function callAppAction(
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory returnedData);

    function callAgreement(
        bytes calldata ctx,
        address agreementClass,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns (bytes memory newCtx, bytes memory cbdata);

    function callAgreement(
        address agreementClass,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory newCtx);

    function callAppAction(
        bytes calldata ctx,
        address app,
        bytes4 selector,
        bytes calldata data
    )
        external
        returns(bytes memory newCtx);

    function chargeGasFee(uint fee) external;
}
