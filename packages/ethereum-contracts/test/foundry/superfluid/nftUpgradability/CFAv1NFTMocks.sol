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

interface ICFAv1NFTBaseMockErrors {
    error STORAGE_LOCATION_CHANGED(string _name);
}

/// @title CFAv1NFTBaseMockV1
/// @author Superfluid
/// @notice A mock CFAv1BaseNFT contract for testing upgradability.
/// @dev This contract *MUST* have the same storage layout as CFAv1NFTBase.sol
/// It is copied and pasted over to remove the extra noise from the functions.
contract CFAv1NFTBaseMockV1 is UUPSProxiable, ICFAv1NFTBaseMockErrors {
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

    function initialize(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        external
        virtual
        initializer // OpenZeppelin Initializable
    {
        superToken = _superToken;

        _name = _nftName;
        _symbol = _nftSymbol;
    }

    /// @notice Validates storage layout
    /// @dev This function is used by all the CFAv1NFTBase mock contracts to validate the layout
    /// It will be the same across all contracts and when upgrading will need to be modified accordingly.
    /// This function only explicitly tests:
    /// - changing the ordering of existing variables
    /// - adding new variables incorrectly
    /// However, it implictly tests the other 3 cases
    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        if (slot != 0 || offset != 2) revert STORAGE_LOCATION_CHANGED("superToken");

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");
        
        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");
        
        assembly { slot := _gap.slot offset := _gap.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_gap");      /// this lasts until slot 50
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

contract CFAv1NFTBaseMockVGoodUpgrade is UUPSProxiable, ICFAv1NFTBaseMockErrors {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;

    string internal _name;
    string internal _symbol;

    mapping(uint256 => address) internal _tokenApprovals;

    mapping(address => mapping(address => bool)) internal _operatorApprovals;

    // @note 3 New variables
    uint256 public newVar1;
    uint256 public newVar2;
    uint256 public newVar3;

    // @note Notice the decrement of gap by the number of new variables added
    uint256[42] private _gap;

    function initialize(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        external
        virtual
        initializer // OpenZeppelin Initializable
    {
        superToken = _superToken;

        _name = _nftName;
        _symbol = _nftSymbol;
    }

    /// @notice Validates storage layout
    /// @dev This function is used by all the CFAv1NFTBase mock contracts to validate the layout
    /// It will be the same across all contracts and when upgrading will need to be modified accordingly.
    /// This function only explicitly tests:
    /// - changing the ordering of existing variables
    /// - adding new variables incorrectly
    /// However, it implictly tests the other 3 cases
    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        if (slot != 0 || offset != 2) revert STORAGE_LOCATION_CHANGED("superToken");

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");
        
        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");
        
        // @note Note how we added three new slot/offset tests for the new storage variables
        assembly { slot := newVar1.slot offset := newVar1.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("newVar1");

        assembly { slot := newVar2.slot offset := newVar2.offset }
        if (slot != 6 || offset != 0) revert STORAGE_LOCATION_CHANGED("newVar2");

        assembly { slot := newVar3.slot offset := newVar3.offset }
        if (slot != 7 || offset != 0) revert STORAGE_LOCATION_CHANGED("newVar3");

        // @note Note how we update the expected slot after adding 3 new variables
        assembly { slot := _gap.slot offset := _gap.offset }
        if (slot != 8 || offset != 0) revert STORAGE_LOCATION_CHANGED("_gap");      /// this lasts until slot 50
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

contract CFAv1NFTBaseMockV1BadNewVariablePreGap is UUPSProxiable, ICFAv1NFTBaseMockErrors {
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

    function initialize(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        external
        initializer // OpenZeppelin Initializable
    {
        superToken = _superToken;

        _name = _nftName;
        _symbol = _nftSymbol;
    }

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
        if (slot != 0 || offset != 2) revert STORAGE_LOCATION_CHANGED("superToken");

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");
        
        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");
        
        assembly { slot := _gap.slot offset := _gap.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_gap");
    }
}

contract CFAv1NFTBaseMockV1BadReorderingPreGap is UUPSProxiable, ICFAv1NFTBaseMockErrors {
    struct FlowData {
        address flowSender;
        address flowReceiver;
    }

    ISuperToken public superToken;
    
    string internal _name;
    string internal _symbol;

    // @note _operatorApprovals and _tokenApprovals switched positions
    mapping(address => mapping(address => bool)) internal _operatorApprovals;
    mapping(uint256 => address) internal _tokenApprovals;

    uint256[45] private _gap;

    function initialize(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        external
        initializer // OpenZeppelin Initializable
    {
        superToken = _superToken;

        _name = _nftName;
        _symbol = _nftSymbol;
    }

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
        if (slot != 0 || offset != 2) revert STORAGE_LOCATION_CHANGED("superToken");

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");
        
        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");
        
        assembly { slot := _gap.slot offset := _gap.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_gap");
    }
}

/*//////////////////////////////////////////////////////////////////////////
                            ConstantOutflowNFT Mocks
//////////////////////////////////////////////////////////////////////////*/

contract ConstantOutflowNFTMockV1 is CFAv1NFTBaseMockV1 {
    mapping(uint256 => FlowData) internal _flowDataByTokenId;

    function proxiableUUID() public pure virtual override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    function validateStorageLayout() public virtual override {
        uint256 slot;
        uint256 offset; // in bytes

        super.validateStorageLayout();

        // slots 5-49 occupied by _gap in CFAv1NFTBaseMockV1

        assembly { slot := _flowDataByTokenId.slot offset := _flowDataByTokenId.offset }
        if (slot != 50 || offset != 0) revert STORAGE_LOCATION_CHANGED("_flowDataByTokenId");   
    }
}

contract CFAv1NFTBaseMockV1BadPostGap is UUPSProxiable, ICFAv1NFTBaseMockErrors {
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

    function initialize(
        ISuperToken _superToken,
        string memory _nftName,
        string memory _nftSymbol
    )
        external
        initializer // OpenZeppelin Initializable
    {
        superToken = _superToken;

        _name = _nftName;
        _symbol = _nftSymbol;
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
    
    function validateStorageLayout() public virtual {
        uint256 slot;
        uint256 offset; // in bytes

        // Initializable._initialized (uint8) 1byte

        // Initializable._initializing (bool) 1byte

        assembly { slot := superToken.slot offset := superToken.offset }
        if (slot != 0 || offset != 2) revert STORAGE_LOCATION_CHANGED("superToken");

        assembly { slot := _name.slot offset := _name.offset }
        if (slot != 1 || offset != 0) revert STORAGE_LOCATION_CHANGED("_name");

        assembly { slot := _symbol.slot offset := _symbol.offset }
        if (slot != 2 || offset != 0) revert STORAGE_LOCATION_CHANGED("_symbol");
        
        assembly { slot := _tokenApprovals.slot offset := _tokenApprovals.offset }
        if (slot != 3 || offset != 0) revert STORAGE_LOCATION_CHANGED("_tokenApprovals");

        assembly { slot := _operatorApprovals.slot offset := _operatorApprovals.offset }
        if (slot != 4 || offset != 0) revert STORAGE_LOCATION_CHANGED("_operatorApprovals");
        
        assembly { slot := _gap.slot offset := _gap.offset }
        if (slot != 5 || offset != 0) revert STORAGE_LOCATION_CHANGED("_gap");
    }
}

contract ConstantOutflowNFTMockV1BaseBadNewVariable is CFAv1NFTBaseMockV1BadPostGap {
    mapping(uint256 => FlowData) internal _flowDataByTokenId;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    function validateStorageLayout() public override {
        uint256 slot;
        uint256 offset; // in bytes

        super.validateStorageLayout();

        // slots 5-49 occupied by _gap in CFAv1NFTBaseMockV1

        assembly { slot := _flowDataByTokenId.slot offset := _flowDataByTokenId.offset }
        if (slot != 50 || offset != 0) revert STORAGE_LOCATION_CHANGED("_flowDataByTokenId");   
    }
}

contract ConstantOutflowNFTMockV1BadNewVariable is CFAv1NFTBaseMockV1 {
    // @note The incorrectly placed variable!
    uint256 public badVariable;
    mapping(uint256 => FlowData) internal _flowDataByTokenId;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    function validateStorageLayout() public override {
        uint256 slot;
        uint256 offset; // in bytes

        super.validateStorageLayout();

        // slots 5-49 occupied by _gap in CFAv1NFTBaseMockV1

        assembly { slot := _flowDataByTokenId.slot offset := _flowDataByTokenId.offset }
        if (slot != 50 || offset != 0) revert STORAGE_LOCATION_CHANGED("_flowDataByTokenId");   
    }
}

/// @title ConstantOutflowNFTMockV1GoodUpgrade
/// @author Superfluid
/// @notice An example of a proper upgrade of the ConstantOutflowNFT contract
/// @dev Notice that the new variable is properly appended to the storage layout
contract ConstantOutflowNFTMockV1GoodUpgrade is ConstantOutflowNFTMockV1 {
    // @note The correctly placed variable!
    uint256 public goodVariable;

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            );
    }

    function validateStorageLayout() public override {
        uint256 slot;
        uint256 offset; // in bytes

        super.validateStorageLayout();

        // slots 5-49 occupied by _gap in CFAv1NFTBaseMockV1

        assembly { slot := _flowDataByTokenId.slot offset := _flowDataByTokenId.offset }
        if (slot != 50 || offset != 0) revert STORAGE_LOCATION_CHANGED("_flowDataByTokenId");   

        assembly { slot := goodVariable.slot offset := goodVariable.offset }
        if (slot != 51 || offset != 0) revert STORAGE_LOCATION_CHANGED("goodVariable");   
    }
}
