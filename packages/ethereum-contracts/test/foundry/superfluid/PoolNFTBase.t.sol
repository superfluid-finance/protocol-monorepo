// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC721Metadata } from "@openzeppelin/contracts/interfaces/IERC721Metadata.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import {
    PoolNFTBaseStorageLayoutMock,
    PoolAdminNFTStorageLayoutMock,
    PoolMemberNFTStorageLayoutMock
} from "../../../contracts/mocks/PoolNFTUpgradabilityMock.sol";
import { IPoolNFTBase, PoolNFTBase } from "../../../contracts/superfluid/PoolNFTBase.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
import { ConstantOutflowNFTMock, ConstantInflowNFTMock } from "../../../contracts/mocks/CFAv1NFTMock.sol";
import { ERC721IntegrationTest } from "./ERC721.t.sol";

/// @title PoolNFTBaseIntegrationTest
/// @author Superfluid
/// @dev This is a base contract for testing PoolNFTBase
/// We test the functions in the PoolNFTBase directly via the base contract
/// and the assumption is that because it is tested here, it is tested for all
/// the derived contracts.
abstract contract PoolNFTBaseIntegrationTest is ERC721IntegrationTest {
    string public constant NAME = "Pool NFT Base";
    string public constant SYMBOL = "PNFTB";
    PoolNFTBase public poolNFTBase;

    function setUp() public virtual override {
        super.setUp();
        poolNFTBase = new PoolNFTBase(sf.host);
        poolNFTBase.initialize(NAME, SYMBOL);
    }

    function _helperGetPoolAdminNftID(address _pool, address _poolAdmin) internal view returns (uint256) {
        return poolAdminNFT.getTokenId(_pool, _poolAdmin);
    }

    function _helperGetPoolMemberNftID(address _pool, address _poolMember) internal view returns (uint256) {
        return poolMemberNFT.getTokenId(_pool, _poolMember);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function _assertPoolAdminNftStateIsExpected(uint256 _tokenId, address _expectedPool, address _expectedAdmin)
        public
    {
        PoolAdminNFT.PoolAdminNFTData memory poolAdminNFTData = poolAdminNFT.poolAdminDataByTokenId(_tokenId);

        assertEq(poolAdminNFTData.pool, _expectedPool, "PoolAdminNFT: pool address not as expected");

        // assert admin is equal to expected admin
        assertEq(poolAdminNFTData.admin, _expectedAdmin, "PoolAdminNFT: admin address not as expected");

        // assert owner of pool admin nft equal to expected admin
        _assertOwnerOfIsExpected(
            poolAdminNFT, _tokenId, _expectedAdmin, "PoolAdminNFT: owner of pool admin nft not as expected"
        );
    }

    function _assertPoolMemberNftStateIsExpected(
        uint256 _tokenId,
        address _expectedPool,
        address _expectedMember,
        uint128 _expectedUnits
    ) public {
        PoolMemberNFT.PoolMemberNFTData memory poolMemberNFTData = poolMemberNFT.poolMemberDataByTokenId(_tokenId);

        assertEq(poolMemberNFTData.pool, _expectedPool, "PoolMemberNFT: pool address not as expected");

        // assert member is equal to expected member
        assertEq(poolMemberNFTData.member, _expectedMember, "PoolMemberNFT: member address not as expected");

        // assert units is equal to expected units
        assertEq(poolMemberNFTData.units, _expectedUnits, "PoolMemberNFT: units not as expected");

        // assert owner of pool member nft equal to expected member
        _assertOwnerOfIsExpected(
            poolAdminNFT, _tokenId, _expectedMember, "PoolMemberNFT: owner of pool member nft not as expected"
        );
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testHostIsProperlySetInConstructor() public {
        assertEq(address(poolNFTBase.HOST()), address(sf.host));
    }

    function testGDAv1IsProperlySetInConstructor() public {
        assertEq(address(poolNFTBase.GENERAL_DISTRIBUTION_AGREEMENT_V1()), address(sf.gda));
    }

    function testBalanceOfIsAlwaysOne(address owner) public {
        assertEq(poolNFTBase.balanceof(owner), 1, "PoolNFTBase: balanceOf is not always one");
    }

    function testContractSupportsExpectedInterfaces() public {
        assertEq(poolNFTBase.supportsInterface(type(IERC165).interfaceId), true);
        assertEq(poolNFTBase.supportsInterface(type(IERC721).interfaceId), true);
        assertEq(poolNFTBase.supportsInterface(type(IERC721Metadata).interfaceId), true);
    }
    
    // TODO
    // create a FlowNFTBase mock contract
    // create a PoolNFTBase mock contract 
    // which allows mock minting and other mock functions
    // so that we can test the functions in the PoolNFTBase directly
    // test initialization worked as expected
    // test approve works as expected
    // test approve reverst if approve to current owner
    // test approve reverts if caller is not owner or approved for all
    // test tokenURI is correct
    // test trigger metadata works as expected
    // test owner of reverts if tokenId is not valid
    // test get approved reverts if not minted
    // test set approval for all works as expected
}

/// @title PoolNFTUpgradabilityTest
/// @author Superfluid
/// @notice Used for testing storage layout and upgradability of Pool NFT contracts
contract PoolNFTUpgradabilityTest is PoolNFTBaseIntegrationTest {
    /*//////////////////////////////////////////////////////////////////////////
                                Storage Layout Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testPoolNFTBaseStorageLayout() public {
        PoolNFTBaseStorageLayoutMock poolNFTBaseStorageLayoutMock = new PoolNFTBaseStorageLayoutMock(sf.host);

        poolNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function testPoolMemberNFTStorageLayout() public {
        PoolMemberNFTStorageLayoutMock poolMemberNFTStorageLayoutMock = new PoolMemberNFTStorageLayoutMock(sf.host);

        poolMemberNFTStorageLayoutMock.validateStorageLayout();
    }

    function testPoolAdminNFTStorageLayout() public {
        PoolAdminNFTStorageLayoutMock poolAdminNFTStorageLayoutMock = new PoolAdminNFTStorageLayoutMock(sf.host);

        poolAdminNFTStorageLayoutMock.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testRevertPoolNFTContractsCannotBeUpgradedByNonSuperTokenFactory(address notSuperTokenFactory) public {
        vm.assume(notSuperTokenFactory != address(sf.superTokenFactory));
        PoolAdminNFT newPoolAdminNFT = new PoolAdminNFT(
            sf.host
        );
        vm.expectRevert(IPoolNFTBase.POOL_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        poolAdminNFT.updateCode(address(newPoolAdminNFT));

        PoolMemberNFT newPoolMemberNFT = new PoolMemberNFT(
            sf.host
        );
        vm.expectRevert(IPoolNFTBase.POOL_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        poolMemberNFT.updateCode(address(newPoolMemberNFT));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testPoolNFTContractsCanBeUpgradedBySuperTokenFactory() public {
        PoolAdminNFT newPoolAdminNFT = new PoolAdminNFT(
            sf.host
        );
        vm.prank(address(sf.superTokenFactory));
        poolAdminNFT.updateCode(address(newPoolAdminNFT));

        PoolMemberNFT newPoolMemberNFT = new PoolMemberNFT(
            sf.host
        );
        vm.prank(address(sf.superTokenFactory));
        poolMemberNFT.updateCode(address(newPoolMemberNFT));
    }
}
