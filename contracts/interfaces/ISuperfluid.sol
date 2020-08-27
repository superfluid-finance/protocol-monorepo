// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

interface ISuperfluid {

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

    function callAppBeforeCallback(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx, bytes memory cbdata);

    function callAppAfterCallback(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx);

    function callAgreementWithContext(
        address agreementClass,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns (bytes memory newCtx, bytes memory returnedData);

    function callAgreement(
        address agreementClass,
        bytes calldata data
    )
        external
        returns(bytes memory returnedData);

    function callAppAction(
        address app,
        bytes calldata data
    )
        external
        returns(bytes memory returnedData);

    function callAppActionWithContext(
        address app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx);

    function chargeGasFee(uint fee) external;
}
