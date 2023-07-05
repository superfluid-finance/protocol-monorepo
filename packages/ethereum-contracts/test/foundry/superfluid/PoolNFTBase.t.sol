// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC721Metadata } from "@openzeppelin/contracts/interfaces/IERC721Metadata.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import {
    PoolNFTBaseStorageLayoutMock,
    PoolAdminNFTStorageLayoutMock,
    PoolMemberNFTStorageLayoutMock
} from "../../../contracts/mocks/PoolNFTUpgradabilityMock.sol";
import { IPoolNFTBase } from "../../../contracts/interfaces/superfluid/IPoolNFTBase.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
import { ConstantOutflowNFTMock, ConstantInflowNFTMock } from "../../../contracts/mocks/CFAv1NFTMock.sol";
import { ERC721IntegrationTest } from "./ERC721.t.sol";

abstract contract PoolNFTBaseIntegrationTest is ERC721IntegrationTest {
    function setUp() public virtual override {
        super.setUp();
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
        assertEq(address(poolAdminNFT.HOST()), address(sf.host));
        assertEq(address(poolMemberNFT.HOST()), address(sf.host));
    }

    function testGDAv1IsProperlySetInConstructor() public {
        assertEq(address(poolAdminNFT.GENERAL_DISTRIBUTION_AGREEMENT_V1()), address(sf.gda));
        assertEq(address(poolMemberNFT.GENERAL_DISTRIBUTION_AGREEMENT_V1()), address(sf.gda));
    }
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
