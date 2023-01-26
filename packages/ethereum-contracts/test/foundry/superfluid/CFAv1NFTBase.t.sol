// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    ConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";

contract ConstantOutflowNFTMock is ConstantOutflowNFT {
    function mockMint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) public {
        _mint(_to, _flowReceiver, _newTokenId);
    }

    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    // @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }
}

contract ConstantInflowNFTMock is ConstantInflowNFT {
    function mockMint(address _to, uint256 _newTokenId) public {
        _mint(_to, _newTokenId);
    }

    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    // @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }
}

abstract contract CFAv1BaseTest is FoundrySuperfluidTester {
    string constant OUTFLOW_NFT_NAME_TEMPLATE = " Constant Outflow NFT";
    string constant OUTFLOW_NFT_SYMBOL_TEMPLATE = "COF";
    string constant INFLOW_NFT_NAME_TEMPLATE = " Constant Inflow NFT";
    string constant INFLOW_NFT_SYMBOL_TEMPLATE = "CIF";

    address public governanceOwner;
    ConstantOutflowNFTMock public constantOutflowNFTLogic;
    ConstantOutflowNFTMock public constantOutflowNFTProxy;
    ConstantInflowNFTMock public constantInflowNFTLogic;
    ConstantInflowNFTMock public constantInflowNFTProxy;

    event Transfer(
        address indexed from,
        address indexed to,
        uint256 indexed tokenId
    );

    event Approval(
        address indexed owner,
        address indexed approved,
        uint256 indexed tokenId
    );

    event ApprovalForAll(
        address indexed owner,
        address indexed operator,
        bool approved
    );

    constructor() FoundrySuperfluidTester(5) {
        governanceOwner = address(sfDeployer);
    }

    function setUp() public override {
        super.setUp();
        (
            constantOutflowNFTLogic,
            constantOutflowNFTProxy,
            constantInflowNFTLogic,
            constantInflowNFTProxy
        ) = helper_deployNFTContractsAndSetAddressInSuperToken();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/
    function helper_deployConstantOutflowNFT()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy
        )
    {
        _constantOutflowNFTLogic = new ConstantOutflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantOutflowNFTLogic));

        _constantOutflowNFTProxy = ConstantOutflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantOutflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_deployConstantInflowNFT()
        public
        returns (
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        _constantInflowNFTLogic = new ConstantInflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantInflowNFTLogic));

        _constantInflowNFTProxy = ConstantInflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantInflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, INFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_deployNFTContractsAndSetAddressInSuperToken()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy,
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        (
            _constantOutflowNFTLogic,
            _constantOutflowNFTProxy
        ) = helper_deployConstantOutflowNFT();
        (
            _constantInflowNFTLogic,
            _constantInflowNFTProxy
        ) = helper_deployConstantInflowNFT();

        vm.prank(governanceOwner);
        superToken.initializeNFTContracts(
            address(_constantOutflowNFTProxy),
            address(_constantInflowNFTProxy),
            address(0),
            address(0)
        );
    }

    function helper_getNFTId(
        address _flowSender,
        address _flowReceiver
    ) public pure returns (uint256) {
        return uint256(keccak256(abi.encode(_flowSender, _flowReceiver)));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Happy Path Cases
    //////////////////////////////////////////////////////////////////////////*/
    function test_NFTContractsDeploymentAndSuperTokenStateInitialization()
        public
    {
        (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy,
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        ) = helper_deployNFTContractsAndSetAddressInSuperToken();
        assertEq(
            address(_constantOutflowNFTProxy),
            address(superToken.constantOutflowNFT())
        );
        assertEq(
            address(_constantInflowNFTProxy),
            address(superToken.constantInflowNFT())
        );
    }
}
