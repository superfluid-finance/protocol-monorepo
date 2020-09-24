// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;
pragma experimental ABIEncoderV2;

import { ISuperfluidGovernance } from "./ISuperfluidGovernance.sol";
import { ISuperToken } from "./ISuperToken.sol";
import { ISuperfluid } from "./ISuperfluid.sol";
import { ISuperAgreement } from "./ISuperAgreement.sol";
import { ISuperApp } from "./ISuperApp.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";


/**
 * @dev Superfluid host interface.

 * It is the central contract of the system where super agreement, super app
 * and super token features are connected together.
 *
 * The superfluid host contract is also the entry point for the protocol users,
 * where batch call and meta transaction are provided for UX improvements.
 *
 * @author Superfluid
 */
interface ISuperfluid {

    /**************************************************************************
     * Governance
     *************************************************************************/
    function getGovernance() external view returns(ISuperfluidGovernance governance);

    // FIXME move to governance
    function addAgreement(address agreement) external;

    // FIXME move to governance
    function isAgreementValid(address agreement) external view returns(bool isValid);

    /**************************************************************************
     * Token Registry
     *************************************************************************/
    function getSuperTokenLogic() external view returns (ISuperToken superToken);

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

    /**************************************************************************
     * App Registry
     *************************************************************************/
    /**
     * @dev Message sender declares it as a super app
     * @param configWord The super app manifest configuration, flags are defined in
     *                   `SuperAppDefinitions`
     */
    function registerApp(uint256 configWord) external;

    /**
     * @dev Query if the app is registered
     * @param app Super app address
     */
    function isApp(ISuperApp app) external view returns(bool);

    /**
     * @dev Query app level
     * @param app Super app address
     */
    function getAppLevel(ISuperApp app) external view returns(uint8 appLevel);

    /**
     * @dev Get the manifest of the super app
     * @param app Super app address
     */
    function getAppManifest(
        ISuperApp app
    )
        external
        view
        returns (
            bool exist,
            uint256 configWord
        );

    /**
     * @dev Query if the app has been jailed
     * @param app Super app address
     */
    function isAppJailed(ISuperApp app) external view returns (bool isJail);

    /**
     * @dev White-list the target app for app composition for the source app (msg.sender)
     * @param targetApp The taget super app address
     */
    function allowCompositeApp(ISuperApp targetApp) external;

    /**
     * @dev Query if source app  is allowed to call the target app as downstream app.
     * @param app Super app address
     * @param targetApp The taget super app address
     */
    function isCompositeAppAllowed(
        ISuperApp app,
        ISuperApp targetApp
    )
        external view returns (bool isAppAllowed);

    event Jail(ISuperApp app, uint256 info);

    /**************************************************************************
     * Agreement Callback System
     *
     * These functions can only be called by registered agreements.
     *************************************************************************/
    function callAppBeforeCallback(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx, bytes memory cbdata);

    function callAppAfterCallback(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx);

    /**************************************************************************
     * Non-app Call Proxies
     *
     * For EOAs or non-app contracts, they are the entry points for interacting
     * with agreements or apps.
     *
     * If the app use these entry points while having an active context, the
     * violating app will be jailed.
     *************************************************************************/
     /**
      * @dev Call agreement function
      * @param data The contextual call data.
      *
      * NOTE: The contextual call data should be generated using
      * abi.encodeWithSelector. The context parameter should be set to "0x",
      * an empty bytes array as a placeholder to be replaced by the host
      * contract.
      */
     function callAgreement(
         ISuperAgreement agreementClass,
         bytes calldata data
     )
        external
        returns(bytes memory returnedData);

    /**
     * @dev Call agreement function
     * @param data The contextual call data.
     *
     * NOTE: See callAgreement about contextual call data.
     */
    function callAppAction(
        ISuperApp app,
        bytes calldata data
    )
        external
        returns(bytes memory returnedData);


    /**
     * @dev Operation type for batch operations
     */
    enum OperationType {
        Approved,         // 0
        TransferFrom,     // 1
        Upgrade,          // 2
        Downgrade,        // 3
        CallAgreement,    // 4
        CallApp           // 5
    }

    /**
     * @dev Batch operation data
     */
    struct Operation {
        // Operation
        OperationType opType;
        // Operation target
        address target;
        // Data specific to operation
        bytes data;
    }

    /**
     * @dev Batch call function.
     * @param operations Array of batch operations.
     */
    function batchCall(Operation[] memory operations) external;

    /**************************************************************************
     * Contextual Call Proxy
     *
     * For apps, they must use context they receive to interact with
     * agreements or apps.
     *
     * The context changes must be saved and returned by the apps in their
     * callbacks always, any modification to the context will be detected and
     * the violating app will be jailed.
     *************************************************************************/
    function callAgreementWithContext(
        ISuperAgreement agreementClass,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns (bytes memory newCtx, bytes memory returnedData);

    function callAppActionWithContext(
        ISuperApp app,
        bytes calldata data,
        bytes calldata ctx
    )
        external
        returns (bytes memory newCtx);

    function chargeGasFee(
        bytes calldata ctx,
        uint fee
    )
        external
        returns (bytes memory newCtx);

    // FIXME this function is under review
    function updateCtxDeposit(
        bytes calldata ctx,
        address receiver,
        uint256 unitOfAllowance
    )
        external
        returns(bytes memory newCtx);

}
