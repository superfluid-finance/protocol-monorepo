// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC165, IERC721, IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { PoolNFTBaseIntegrationTest } from "./PoolNFTBase.t.sol";
import { IPoolNFTBase } from "../../../contracts/interfaces/superfluid/IPoolNFTBase.sol";
import { IPoolAdminNFT } from "../../../contracts/interfaces/superfluid/IPoolAdminNFT.sol";
import { ISuperfluidPool } from "../../../contracts/superfluid/SuperfluidPool.sol";

contract PoolAdminNFTIntegrationTest is PoolNFTBaseIntegrationTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testRevertIfTransferFrom(address _poolAdmin, address _receiver) public {
        vm.assume(_poolAdmin != address(0));
        vm.assume(_receiver != address(0));

        ISuperfluidPool pool = sf.gda.createPool(superTokenMock, _poolAdmin);
        uint256 nftId = _helperGetPoolAdminNftId(address(pool), _poolAdmin);

        _helperMockMint(address(pool));

        _helperRevertIfTransferFrom(
            poolAdminNFT,
            _poolAdmin,
            _poolAdmin,
            _receiver,
            nftId,
            IPoolNFTBase.POOL_NFT_TRANSFER_NOT_ALLOWED.selector
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

    function _helperMockMint(address _pool) internal returns (uint256) {
        address poolAdmin = ISuperfluidPool(_pool).admin();

        uint256 tokenId = _helperGetPoolAdminNftId(_pool, poolAdmin);

        _assertEventTransfer(address(poolAdminNFT), address(0), poolAdmin, tokenId);
        poolAdminNFT.mockMint(_pool);
        return tokenId;
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            poolAdminNFT.proxiableUUID(), keccak256("org.superfluid-finance.contracts.PoolAdminNFT.implementation")
        );
    }
}

contract FakePool {
    address public admin;
    address public superToken;

    constructor(address _admin, address _superToken) {
        admin = _admin;
        superToken = _superToken;
    }
}
