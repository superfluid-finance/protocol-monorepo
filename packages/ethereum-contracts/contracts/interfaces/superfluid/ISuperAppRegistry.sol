// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;
import { ISuperApp } from "./ISuperApp.sol";


/**
 * @title Super App Registry
 * @author Superfluid
 *
 * NOTE:
 * The superfluid host implements the Super App Registry interface, but it might have a white-list.
 * To register as an app for such host contract, a delegate may be trusted by the host, and implements this registry
 * inteface instead.
 *
 */
interface ISuperAppRegistry {

    /**
    * @dev Message sender declares it as a super app
    * @param configWord The super app manifest configuration, flags are defined in
    * `SuperAppDefinitions`
    */
    function registerApp(uint256 configWord) external;

    /**
    * @dev App registered event
    * @param app Address of jailed app
    */
    event AppRegistered(ISuperApp indexed app);

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
        external view
        returns (
            bool isSuperApp,
            bool isJailed,
            uint256 noopMask
        );

    /**
     * @dev Query if the app has been jailed
     * @param app Super app address
     */
    function isAppJailed(ISuperApp app) external view returns (bool isJail);

    /**
     * @dev Whitelist the target app for app composition for the source app (msg.sender)
     * @param targetApp The target super app address
     */
    function allowCompositeApp(ISuperApp targetApp) external;

    /**
     * @dev Query if source app is allowed to call the target app as downstream app
     * @param app Super app address
     * @param targetApp The target super app address
     */
    function isCompositeAppAllowed(
        ISuperApp app,
        ISuperApp targetApp
    )
        external view
        returns (bool isAppAllowed);

}
