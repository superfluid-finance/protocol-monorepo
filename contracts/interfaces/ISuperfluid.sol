// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;
pragma experimental ABIEncoderV2;

import { ISuperToken } from "./ISuperToken.sol";
import { ISuperfluidGovernance } from "./ISuperfluidGovernance.sol";
import { ISuperfluid } from "./ISuperfluid.sol";
import { ISuperfluid } from "./ISuperfluid.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

interface ISuperfluid {

    enum TypeOperation {
        Approved,
        TransferFrom,
        Upgrade,
        Downgrade,
        CallAgreement,
        CallApp
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
    returns (ISuperToken superToken);

    function getGovernance() external view returns(ISuperfluidGovernance governance);

    function getSuperTokenLogic() external view returns (ISuperToken superToken);

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

    function isAppJailed(address app) external view returns (bool isJail);

    /**
     * @notice White-list the target app for app composition for the source app `msg.sender`
     * @param targetApp The taget super app address
     */
    function allowCompositeApp(address targetApp) external;

    function isCompositeAppAllowed(address app, address targetApp) external view returns (bool isAppAllowed);

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

    function isApp(address app) external view returns(bool);

    function updateCtxDeposit(
        bytes calldata ctx,
        address receiver,
        uint256 unitOfAllowance
    )
        external
        returns(bytes memory newCtx);

    function getAppLevel(address appAddr) external view returns(uint8 appLevel);

    function addAgreement(address agreement) external;

    function isAgreementValid(address agreement) external view returns(bool isValid);
}
