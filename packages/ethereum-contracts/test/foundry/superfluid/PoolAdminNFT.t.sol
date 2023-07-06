// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC165, IERC721, IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";
import { PoolNFTBaseIntegrationTest, FakePool } from "./PoolNFTBase.t.sol";
import { IPoolNFTBase } from "../../../contracts/interfaces/superfluid/IPoolNFTBase.sol";
import { IPoolAdminNFT } from "../../../contracts/interfaces/superfluid/IPoolAdminNFT.sol";
import { ISuperfluidPool } from "../../../contracts/superfluid/SuperfluidPool.sol";

contract PoolAdminNFTIntegrationTest is PoolNFTBaseIntegrationTest {
    using Strings for uint256;

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testRevertIfTransferFromForPoolAdminNFT(address _poolAdmin, address _receiver) public {
        vm.assume(_poolAdmin != address(0));
        vm.assume(_receiver != address(0));
        vm.assume(_poolAdmin != _receiver);

        ISuperfluidPool pool = sf.gda.createPool(superTokenMock, _poolAdmin);
        uint256 nftId = _helperGetPoolAdminNftId(address(pool), _poolAdmin);

        _helperRevertIfTransferFrom(
            poolAdminNFT, _poolAdmin, _poolAdmin, _receiver, nftId, IPoolNFTBase.POOL_NFT_TRANSFER_NOT_ALLOWED.selector
        );
    }

    function testRevertIfMintingForFakePool() public {
        FakePool pool = new FakePool(alice, address(superTokenMock));
        vm.expectRevert(IPoolNFTBase.POOL_NFT_NOT_REGISTERED_POOL.selector);
        poolAdminNFT.mockMint(address(pool));
    }

    function testRevertIfMintingForNotPool(address _pool) public {
        vm.expectRevert();
        poolAdminNFT.mockMint(_pool);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            poolAdminNFT.proxiableUUID(), keccak256("org.superfluid-finance.contracts.PoolAdminNFT.implementation")
        );
    }

    function testTokenURIForPoolAdminNFT(uint256 tokenId) public {
        assertEq(poolAdminNFT.tokenURI(tokenId), string(abi.encodePacked(poolAdminNFT.baseURI())));
    }

}
