// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;
pragma experimental ABIEncoderV2;

import { IERC20, ISuperToken } from "./ISuperToken.sol";
import { ISuperfluidGovernance } from "./ISuperfluidGovernance.sol";


interface ISuperfluid {

    enum TypeOperation {
        Transfer, //0
        Upgrade, //1
        Downgrade, //2
        CallAgreement, //3
        CallApp //4
    }

    struct Operation {
        TypeOperation opType;
        address call;
        bytes data;
    }

    function getERC20Wrapper(
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    returns (address wrapperAddress, bool created);

    function createERC20Wrapper(
        string calldata name,
        string calldata symbol,
        uint8 decimals,
        IERC20 token
    )
    external
    returns (ISuperToken);

    function getGovernance() external returns (ISuperfluidGovernance);

    function getSuperTokenLogic() external returns (ISuperToken);

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

    function callBatch(Operation[] memory operations) external;

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
