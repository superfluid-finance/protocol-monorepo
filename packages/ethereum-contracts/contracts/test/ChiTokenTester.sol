// SPDX-License-Identifier: AGPLv3
// Copy and adapted from: https://github.com/1inch-exchange/chi/blob/master/contracts/TestHelper.sol
pragma solidity 0.7.6;

import "../interfaces/tokens/IChiToken.sol";


contract ChiTokenTester {

    function dummy() public pure {
        assembly{
            invalid()
        }
    }

    // Burns at least burn gas by calling itself and throwing
    function burnGas(uint256 burn) public {
        uint ret;
        // call self.dummy() to burn a bunch of gas
        assembly {
            mstore(0x0, 0x32e43a1100000000000000000000000000000000000000000000000000000000)
            ret := call(burn, address(), 0, 0x0, 0x04, 0x0, 0)
        }
        assert(ret == 0);
    }

    function burnGasAndFree(address gasToken, uint256 burn, uint256 free) public {
        burnGas(burn);
        require(IChiToken(gasToken).free(free) > 0, "burnGasAndFree");
    }

    function burnGasAndFreeUpTo(address gasToken, uint256 burn, uint256 free) public {
        burnGas(burn);
        require(free == IChiToken(gasToken).freeUpTo(free), "burnGasAndFreeUpTo");
    }

    function burnGasAndFreeFrom(address gasToken, uint256 burn, uint256 free) public {
        burnGas(burn);
        require(IChiToken(gasToken).freeFrom(msg.sender, free) > 0, "burnGasAndFreeFrom");
    }

    function burnGasAndFreeFromUpTo(address gasToken, uint256 burn, uint256 free) public {
        burnGas(burn);
        require(free == IChiToken(gasToken).freeFromUpTo(msg.sender, free), "burnGasAndFreeFromUpTo");
    }
}
