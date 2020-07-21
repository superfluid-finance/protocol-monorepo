pragma solidity >=0.5.0;

import "./Proxy.sol";

contract Proxiable {
    // Code position in storage is
    // keccak256("PROXIABLE") = "0xc5f16f0fcc639fa48a6947836d9850f504798523bf8c9a3a87d5876cf622bcf7"
    bytes32 internal constant _IMPLEMENTATION_SLOT = 0xc5f16f0fcc639fa48a6947836d9850f504798523bf8c9a3a87d5876cf622bcf7;

    function _updateCodeAddress(address newAddress) internal {
        require(
            bytes32(
                    _IMPLEMENTATION_SLOT
                ) ==
                Proxiable(newAddress).proxiableUUID(),
            "Not compatible"
        );
        assembly {
            // solium-disable-line
            sstore(
                _IMPLEMENTATION_SLOT,
                newAddress
            )
        }
    }

    function getCodeAddress() external view returns (address codeAddress){
        assembly {
            // solium-disable-line
            codeAddress := sload(
                _IMPLEMENTATION_SLOT
            )
        }
    }

    function proxiableUUID() external pure returns (bytes32) {
        return
            _IMPLEMENTATION_SLOT;
    }
}
