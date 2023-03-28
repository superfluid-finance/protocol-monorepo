// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

// @note THIS IS A TEMPORARY CONTRACT SOLELY FOR THE DEMO

import {
    IGeneralDistributionAgreementV1,
    ISuperfluidToken,
    ISuperTokenPool
} from "../interfaces/agreements/IGeneralDistributionAgreementV1.sol";

contract GDADeployer {
    IGeneralDistributionAgreementV1 public immutable _gda;
    ISuperfluidToken public immutable _superToken;
    address public immutable _owner;
    ISuperTokenPool public pool;

    error NOT_OWNER();
    error UNEQUAL_LENGTH_ARRAY();
    
    event PoolCreated(address pool);

    constructor(
        IGeneralDistributionAgreementV1 gda,
        ISuperfluidToken superToken
    ) {
        _gda = gda;
        _owner = msg.sender;
        _superToken = superToken;
    }

    /// @dev Creates a pool with this contract as the admin
    function createPool() external {
        if (msg.sender != _owner) revert NOT_OWNER();
        pool = _gda.createPool(address(this), _superToken);

        emit PoolCreated(address(pool));
    }

    /// @dev Updates member units for an array of members
    /// @param members member addresses
    /// @param units unit allocation for members
    function updateMemberUnits(
        address[] calldata members,
        int128[] calldata units
    ) external {
        if (members.length != units.length) revert UNEQUAL_LENGTH_ARRAY();
        for (uint256 i; i < members.length; ) {
            pool.updateMember(members[i], units[i]);
            unchecked {
                ++i;
            }
        }
    }
}
