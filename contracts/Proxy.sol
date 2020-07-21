// SPDX-License-Identifier: MIT
pragma solidity ^0.6.6;

contract Proxy {

    // Code position in storage is:
    // keccak256("PROXIABLE") = "0xc5f16f0fcc639fa48a6947836d9850f504798523bf8c9a3a87d5876cf622bcf7"
    bytes32 internal constant _IMPLEMENTATION_SLOT = 0xc5f16f0fcc639fa48a6947836d9850f504798523bf8c9a3a87d5876cf622bcf7;

    // @dev Empty constructor is more friendlier to CREATE2, otherwise parameters will be part of the
    // address computation. Use initalizeProxy for initializing the logic contract address.
    function initializeProxy(address initialAddress) external {
        address codeAddress;
        assembly {
            // solium-disable-line
            codeAddress := sload(
                _IMPLEMENTATION_SLOT
            )
        }
        require(codeAddress == address(0), "Proxy: ALREADY_INITIALIZED");
        assembly {
            // solium-disable-line
            sstore(

                _IMPLEMENTATION_SLOT,
                initialAddress
            )
        }
    }

    fallback () external payable {
        assembly { // solium-disable-line
            let implementation := sload(_IMPLEMENTATION_SLOT)

            // Copy msg.data. We take full control of memory in this inline assembly
            // block because it will not return to Solidity code. We overwrite the
            // Solidity scratch pad at memory position 0.
            calldatacopy(0, 0, calldatasize())

            // Call the implementation.
            // out and outsize are 0 because we don't know the size yet.
            let result := delegatecall(gas(), implementation, 0, calldatasize(), 0, 0)

            // Copy the returned data.
            returndatacopy(0, 0, returndatasize())

            switch result
            // delegatecall returns 0 on error.
            case 0 { revert(0, returndatasize()) }
            default { return(0, returndatasize()) }
        }
    }
}
