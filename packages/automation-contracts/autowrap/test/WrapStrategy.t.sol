// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { SuperfluidFrameworkDeployer, SuperfluidTester, Superfluid, ConstantFlowAgreementV1, CFAv1Library } from "../test/SuperfluidTester.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ERC1820RegistryCompiled } from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";
import { Manager } from "./../contracts/Manager.sol";
import { WrapStrategy } from "./../contracts/strategies/WrapStrategy.sol";
import { IStrategy } from "./../contracts/interfaces/IStrategy.sol";
import { IManager } from "./../contracts/interfaces/IManager.sol";


/// @title ManagerTests
contract WrapStrategyTests is SuperfluidTester {

    event Wrap(
        address indexed user,
        address indexed superToken,
        uint256 superTokenAmount
    );
    event ManagerChanged(
        address indexed oldManager,
        address indexed Manager
    );
    event EmergencyWithdrawInitiated(
        address indexed receiver,
        address indexed token,
        uint256 amount
    );

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;
    Superfluid host;
    ConstantFlowAgreementV1 cfa;
    uint256 private _expectedTotalSupply = 0;
    Manager public manager;
    WrapStrategy public wrapStrategy;
    ISuperToken nativeSuperToken;

    /// @dev This is required by solidity for using the CFAv1Library in the tester
    using CFAv1Library for CFAv1Library.InitData;

    /// @dev Constants for Testing

    uint64 constant MIN_LOWER = 2;
    uint64 constant MIN_UPPER = 7;
    uint64 constant EXPIRY = type(uint64).max;

    constructor() SuperfluidTester(3) {
        vm.startPrank(admin);
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);
        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();
        host = sf.host;
        cfa = sf.cfa;
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                    )
                )
            )
        );
        manager = new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        wrapStrategy = new WrapStrategy(address(manager));
        vm.stopPrank();
    }

    /// SETUP AND HELPERS

    function setUp() public virtual {
        (token, superToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);

        for (uint32 i = 0; i < N_TESTERS; ++i) {
            token.mint(TEST_ACCOUNTS[i], INIT_TOKEN_BALANCE);
            vm.startPrank(TEST_ACCOUNTS[i]);
            token.approve(address(superToken), INIT_SUPER_TOKEN_BALANCE);
            superToken.upgrade(INIT_SUPER_TOKEN_BALANCE);
            _expectedTotalSupply += INIT_SUPER_TOKEN_BALANCE;
            vm.stopPrank();
        }

        nativeSuperToken = sfDeployer.deployNativeAssetSuperToken("xFTT", "xFTT");
        manager = new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        wrapStrategy = new WrapStrategy(address(manager));
    }

    function getWrapIndex(
        address user,
        address superToken,
        address liquidityToken
    ) public pure returns (bytes32) {
        return keccak256(abi.encode(user, superToken, liquidityToken));
    }

    function startStream(address sender, address receiver, int96 flowRate) public {
        vm.startPrank(sender);
        cfaV1.createFlow(receiver, superToken, flowRate);
        vm.stopPrank();
    }

    function stopStream(address sender, address receiver) public {
        vm.startPrank(sender);
        cfaV1.deleteFlow(sender, receiver, superToken);
        vm.stopPrank();
    }


    /// TESTS

    function testFailDeploymentWithoutManager() public {
        new WrapStrategy(address(0));
    }

    function testDeploymentCheckData() public {
        WrapStrategy wrap = new WrapStrategy(address(manager));
        assertEq(address(manager), wrap.manager(), "manager address not equal");
    }

    // Manager operations

    function testCannotChangeManagerContractIfNotOwner() public {
        Manager newManager =  new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        vm.prank(admin);
        vm.expectRevert(bytes("Ownable: caller is not the owner"));
        wrapStrategy.changeManager(address(newManager));
    }

    function testCannotChangeManagerZeroAddress() public {
        vm.expectRevert(IStrategy.ZeroAddress.selector);
        wrapStrategy.changeManager(address(0));
    }

    function testChangeManagerContract() public {
        Manager newManager =  new Manager(address(cfa), MIN_LOWER, MIN_UPPER);
        vm.expectEmit(true, true, true, true);
        emit ManagerChanged(address(manager), address(newManager));
        wrapStrategy.changeManager(address(newManager));
    }

    // SuperToken

    function testSupportedSuperToken() public {
        assertTrue(
            wrapStrategy.isSupportedSuperToken(superToken),
            "SuperToken should be supported"
        );
    }

    function testCannotNonSupportedSuperToken() public {
        assertTrue(
            !wrapStrategy.isSupportedSuperToken(nativeSuperToken),
            "Native SuperToken shouldn't be supported"
        );
    }

    // Wrap Strategy Operations

    function testCannotPerformWrapIfNotManager() public {
        vm.prank(alice);
        vm.expectRevert(abi.encodeWithSelector(IStrategy.UnauthorizedCaller.selector, alice, address(manager)));
        wrapStrategy.wrap(bob, superToken, 1);
    }

}
