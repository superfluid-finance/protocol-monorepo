// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { Test } from "forge-std/Test.sol";

import {
    ISuperToken
} from "../../../../contracts/interfaces/superfluid/ISuperToken.sol";

import {
    UUPSProxiable
} from "../../../../contracts/upgradability/UUPSProxiable.sol";

import { CFAv1NFTBase } from "../CFAv1NFTBase.t.sol";

/*//////////////////////////////////////////////////////////////////////////
                                CFAv1NFTBase Mocks
//////////////////////////////////////////////////////////////////////////*/

/// @title CFAv1NFTBaseMockV1
/// @author Superfluid
/// @notice A mock CFAv1BaseNFT contract for testing upgradability.
/// @dev This contract *MUST* have the same storage layout as CFAv1NFTBase.sol
/// It is copied and pasted over to remove the extra noise from the functions.
contract CFAv1NFTBaseMockV1 is UUPSProxiable {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    mapping(uint256 => address) internal _tokenApprovals;

    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    uint256[45] private _gap;

    ///
    function validateStorageLayout() public {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        assert(slot == 0);
        assert(offset == 2);

        assembly { slot := _name.slot offset := _name.offset }
        assert(slot == 1);
        assert(offset == 0);

        assembly { slot := _symbol.slot offset := _symbol.offset }
        assert(slot == 2);
        assert(offset == 0);

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        assert(slot == 3);
        assert(offset == 0);

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        assert(slot == 4);
        assert(offset == 0);

        assembly { slot := _gap.slot offset := _gap.offset }
        assert(slot == 5);
        assert(offset == 0);
    }

    function proxiableUUID() public pure virtual override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.CFAv1NFTBase.implementation"
            );
    }

    function updateCode(address newAddress) external override {
        if (msg.sender != address(superToken.getHost())) {
            revert CFAv1NFTBase.CFA_NFT_ONLY_HOST();
        }

        UUPSProxiable._updateCodeAddress(newAddress);
    }
}

contract CFAv1NFTBaseMockV1BadPreGap is UUPSProxiable {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;
    
    // @note The incorrectly placed variable!
    uint256 public badVariable;

    string internal _name;
    string internal _symbol;

    mapping(uint256 => address) internal _tokenApprovals;

    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    uint256[45] private _gap;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.CFAv1NFTBase.implementation"
            );
    }

    function updateCode(address newAddress) external override {
        if (msg.sender != address(superToken.getHost())) {
            revert CFAv1NFTBase.CFA_NFT_ONLY_HOST();
        }

        UUPSProxiable._updateCodeAddress(newAddress);
    }

    function validateStorageLayout() public {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        assert(slot == 0);
        assert(offset == 2);

        assembly { slot := _name.slot offset := _name.offset }
        assert(slot == 1);
        assert(offset == 0);

        assembly { slot := _symbol.slot offset := _symbol.offset }
        assert(slot == 2);
        assert(offset == 0);

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        assert(slot == 3);
        assert(offset == 0);

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        assert(slot == 4);
        assert(offset == 0);

        assembly { slot := _gap.slot offset := _gap.offset }
        assert(slot == 5);
        assert(offset == 0);
    }
}

contract CFAv1NFTBaseMockV1BadPostGap is UUPSProxiable {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    mapping(uint256 => address) internal _tokenApprovals;

    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    uint256[45] private _gap;

    // @note The incorrectly placed variable!
    uint256 public badVariable;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.CFAv1NFTBase.implementation"
            );
    }
    
    function updateCode(address newAddress) external override {
        if (msg.sender != address(superToken.getHost())) {
            revert CFAv1NFTBase.CFA_NFT_ONLY_HOST();
        }

        UUPSProxiable._updateCodeAddress(newAddress);
    }
    
    function validateStorageLayout() public {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        assert(slot == 0);
        assert(offset == 2);

        assembly { slot := _name.slot offset := _name.offset }
        assert(slot == 1);
        assert(offset == 0);

        assembly { slot := _symbol.slot offset := _symbol.offset }
        assert(slot == 2);
        assert(offset == 0);

        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        assert(slot == 3);
        assert(offset == 0);

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        assert(slot == 4);
        assert(offset == 0);

        assembly { slot := _gap.slot offset := _gap.offset }
        assert(slot == 5);
        assert(offset == 0);
    }
}

// Inherit this in ConstantOutflowNFT

contract CFAv1NFTBaseMockV1Good is UUPSProxiable {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    mapping(uint256 => address) internal _tokenApprovals;

    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    // TODO: Put something here and minus length of gap array by 1
    uint256[45] private _gap;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.CFAv1NFTBase.implementation"
            );
    }
    
    function updateCode(address newAddress) external override {
        if (msg.sender != address(superToken.getHost())) {
            revert CFAv1NFTBase.CFA_NFT_ONLY_HOST();
        }

        UUPSProxiable._updateCodeAddress(newAddress);
    }
}

/*//////////////////////////////////////////////////////////////////////////
                            ConstantOutflowNFT Mocks
//////////////////////////////////////////////////////////////////////////*/

contract ConstantOutflowNFTMockV1 is CFAv1NFTBaseMockV1 {
    mapping(uint256 => FlowData) internal _flowDataBySenderReceiver;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }
}

contract ConstantOutflowNFTMockV1Bad is CFAv1NFTBaseMockV1 {
    // TODO: Put something here
    mapping(uint256 => FlowData) internal _flowDataBySenderReceiver;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }
}

contract ConstantOutflowNFTMockV1Good is CFAv1NFTBaseMockV1 {
    mapping(uint256 => FlowData) internal _flowDataBySenderReceiver;
    // TODO: Put something here

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }
}
