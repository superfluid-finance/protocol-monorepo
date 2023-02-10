// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.4;

import { ISuperToken } from "./ISuperToken.sol";

import {
    IERC20,
    ERC20WithTokenInfo
} from "../tokens/ERC20WithTokenInfo.sol";
import { IConstantOutflowNFT } from "./IConstantOutflowNFT.sol";
import { IConstantInflowNFT } from "./IConstantInflowNFT.sol";
import { IPoolAdminNFT } from "./IPoolAdminNFT.sol";
import { IPoolMemberNFT } from "./IPoolMemberNFT.sol";

/**
 * @title Super token factory interface
 * @author Superfluid
 */
interface ISuperTokenFactory {

    /**************************************************************************
     * Errors
     *************************************************************************/
    error SUPER_TOKEN_FACTORY_ALREADY_EXISTS(); // 0x91d67972
    error SUPER_TOKEN_FACTORY_DOES_NOT_EXIST(); // 0x872cac48
    error SUPER_TOKEN_FACTORY_UNINITIALIZED();  // 0x1b39b9b4
    error SUPER_TOKEN_FACTORY_ONLY_HOST();      // 0x478b8e83
    error SUPER_TOKEN_FACTORY_ZERO_ADDRESS();   // 0x305c9e82

    /**
     * @dev Get superfluid host contract address
     */
    function getHost() external view returns(address host);

    /// @dev Initialize the contract
    function initialize() external;

    /**
     * @dev Get the current super token logic used by the factory
     */
    function getSuperTokenLogic() external view returns (ISuperToken superToken);

    /**
     * @dev Get the current constant outflow NFT logic used by the factory
     */
    function getConstantOutflowNFTLogic() external view returns (IConstantOutflowNFT constantOutflowNFT);

    /**
     * @dev Get the current constant inflow NFT logic used by the factory
     */
    function getConstantInflowNFTLogic() external view returns (IConstantInflowNFT constantInflowNFT);

    /**
     * @dev Upgradability modes
     */
    enum Upgradability {
        /// Non upgradable super token, `host.updateSuperTokenLogic` will revert
        NON_UPGRADABLE,
        /// Upgradable through `host.updateSuperTokenLogic` operation
        SEMI_UPGRADABLE,
        /// Always using the latest super token logic
        FULL_UPGRADABLE
    }

    /**
     * @notice Create new super token wrapper for the underlying ERC20 token
     * @param underlyingToken Underlying ERC20 token
     * @param underlyingDecimals Underlying token decimals
     * @param upgradability Upgradability mode
     * @param name Super token name
     * @param symbol Super token symbol
     * @return superToken The deployed and initialized wrapper super token
     */
    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external
        returns (ISuperToken superToken);

    /**
     * @notice Create new super token wrapper for the underlying ERC20 token with extra token info
     * @param underlyingToken Underlying ERC20 token
     * @param upgradability Upgradability mode
     * @param name Super token name
     * @param symbol Super token symbol
     * @return superToken The deployed and initialized wrapper super token
     * NOTE:
     * - It assumes token provide the .decimals() function
     */
    function createERC20Wrapper(
        ERC20WithTokenInfo underlyingToken,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external
        returns (ISuperToken superToken);

    /**
     * @notice Creates a wrapper super token AND sets it in the canonical list OR reverts if it already exists
     * @dev salt for create2 is the keccak256 hash of abi.encode(address(_underlyingToken))
     * @param _underlyingToken Underlying ERC20 token
     * @return ISuperToken the created supertoken
     */
    function createCanonicalERC20Wrapper(ERC20WithTokenInfo _underlyingToken)
        external
        returns (ISuperToken);

    /**
     * @notice Computes/Retrieves wrapper super token address given the underlying token address
     * @dev We return from our canonical list if it already exists, otherwise we compute it
     * @dev note that this function only computes addresses for SEMI_UPGRADABLE SuperTokens
     * @param _underlyingToken Underlying ERC20 token address
     * @return superTokenAddress Super token address
     * @return isDeployed whether the super token is deployed AND set in the canonical mapping
     */
    function computeCanonicalERC20WrapperAddress(address _underlyingToken)
        external
        view
        returns (address superTokenAddress, bool isDeployed);

    /**
     * @notice Gets the canonical ERC20 wrapper super token address given the underlying token address
     * @dev We return the address if it exists and the zero address otherwise
     * @param _underlyingTokenAddress Underlying ERC20 token address
     * @return superTokenAddress Super token address
     */
    function getCanonicalERC20Wrapper(address _underlyingTokenAddress)
        external
        view
        returns (address superTokenAddress);

    /**
     * @dev Creates a new custom super token
     * @param customSuperTokenProxy address of the custom supertoken proxy
     */
    function initializeCustomSuperToken(
        address customSuperTokenProxy
    )
        external;

    /**
     * @notice Deploys the NFT proxy contracts and initializes them
     * @dev This function still requires you to call SuperToken.initializeNFTContracts
     * to link the NFT contracts to the super token
     * NOTE: This function is only callable by the governance contract owner
     * @param superToken the super token we are attaching the NFT contracts to
     * @param constantOutflowNFTLogic address of the constant outflow NFT logic contract
     * @param constantInflowNFTLogic address of the constant inflow NFT logic contract
     * @param poolAdminNFTProxy address of the pool admin NFT proxy contract
     * @param poolMemberNFT address of the pool member NFT contract
     * @return constantOutflowNFT the deployed constant outflow NFT contract
     * @return constantInflowNFT the deployed constant inflow NFT contract
     * @return poolAdminNFT the deployed pool admin NFT contract
     * @return poolMemberNFT the deployed pool member NFT contract
     */
    function deployNFTProxyContractsAndInititialize(
        ISuperToken superToken,
        address constantOutflowNFTLogic,
        address constantInflowNFTLogic,
        address poolAdminNFTProxy,
        address poolMemberNFTProxy
    )
        external
        returns (
            IConstantOutflowNFT constantOutflowNFT,
            IConstantInflowNFT constantInflowNFT,
            IPoolAdminNFT poolAdminNFT,
            IPoolMemberNFT poolMemberNFT
        );

    /**
     * @dev Super token logic created event
     * @param tokenLogic Token logic address
     */
    event SuperTokenLogicCreated(ISuperToken indexed tokenLogic);

    /**
     * @dev Super token created event
     * @param token Newly created super token address
     */
    event SuperTokenCreated(ISuperToken indexed token);

    /**
     * @dev Custom super token created event
     * @param token Newly created custom super token address
     */
    event CustomSuperTokenCreated(ISuperToken indexed token);

    /**
     * @dev Constant Outflow NFT logic created event
     * @param constantOutflowNFTLogic constant outflow nft logic address
     */
    event ConstantOutflowNFTLogicCreated(
        IConstantOutflowNFT indexed constantOutflowNFTLogic
    );

    /**
     * @dev Constant Outflow NFT proxy created event
     * @param constantOutflowNFT constant outflow nft address
     */
    event ConstantOutflowNFTCreated(
        IConstantOutflowNFT indexed constantOutflowNFT
    );

    /**
     * @dev Constant Inflow NFT logic created event
     * @param constantInflowNFTLogic constant inflow nft logic address
     */
    event ConstantInflowNFTLogicCreated(
        IConstantInflowNFT indexed constantInflowNFTLogic
    );

    /**
     * @dev Constant Inflow NFT proxy created event
     * @param constantInflowNFT constant inflow nft address
     */
    event ConstantInflowNFTCreated(
        IConstantInflowNFT indexed constantInflowNFT
    );

    /**
     * @dev Pool Admin NFT logic created event
     * @param poolAdminNFTProxy pool admin nft proxy address
     */
    event PoolAdminNFTCreated(IPoolAdminNFT indexed poolAdminNFTProxy);

    /**
     * @dev Pool Admin NFT logic created event
     * @param poolAdminNFTProxy pool admin nft proxy address
     */
    event PoolAdminNFTLogicCreated(IPoolAdminNFT indexed poolAdminNFTProxy);

    /**
     * @dev Pool Member NFT logic created event
     * @param poolMemberNFTProxy pool member nft proxy address
     */
    event PoolMemberNFTCreated(IPoolMemberNFT indexed poolMemberNFTProxy);

    /**
     * @dev Pool Member NFT logic created event
     * @param poolMemberNFTProxy pool member nft proxy address
     */
    event PoolMemberNFTLogicCreated(IPoolMemberNFT indexed poolMemberNFTProxy);
}
