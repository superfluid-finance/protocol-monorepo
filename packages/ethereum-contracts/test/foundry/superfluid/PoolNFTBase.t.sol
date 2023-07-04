// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { SuperToken, SuperTokenMock } from "../../../contracts/mocks/SuperTokenMock.sol";
import { PoolNFTBaseStorageLayoutMock, PoolAdminNFTStorageLayoutMock, PoolMemberNFTStorageLayoutMock } from "../../../contracts/mocks/PoolNFTUpgradabilityMock.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
import { ConstantOutflowNFTMock, ConstantInflowNFTMock } from "../../../contracts/mocks/CFAv1NFTMock.sol";
import { PoolAdminNFTMock, PoolMemberNFTMock } from "../../../contracts/mocks/PoolNFTMock.sol";

abstract contract PoolNFTBaseIntegrationTest is FoundrySuperfluidTester {
    SuperTokenMock public superTokenMock;

    PoolMemberNFTMock public poolMemberNFTLogic;
    PoolAdminNFTMock public poolAdminNFTLogic;

    PoolMemberNFTMock public poolMemberNFT;
    PoolAdminNFTMock public poolAdminNFT;

    constructor() FoundrySuperfluidTester(5) { }

    function setUp() public virtual override {
        super.setUp();

        // Deploy Flow NFTs

        // deploy outflow NFT contract
        UUPSProxy outflowProxy = new UUPSProxy();

        // deploy inflow NFT contract
        UUPSProxy inflowProxy = new UUPSProxy();

        // we deploy mock NFT contracts for the tests to access internal functions
        ConstantOutflowNFTMock constantOutflowNFTLogic = new ConstantOutflowNFTMock(
            sf.host,
            IConstantInflowNFT(address(inflowProxy))
        );
        ConstantInflowNFTMock constantInflowNFTLogic = new ConstantInflowNFTMock(
            sf.host,
            IConstantOutflowNFT(address(outflowProxy))
        );

        constantOutflowNFTLogic.castrate();
        constantInflowNFTLogic.castrate();

        ConstantOutflowNFTMock constantOutflowNFT = ConstantOutflowNFTMock(address(outflowProxy));
        ConstantInflowNFTMock constantInflowNFT = ConstantInflowNFTMock(address(inflowProxy));

        constantOutflowNFT.initialize("Constant Outflow NFT", "COF");

        constantInflowNFT.initialize("Constant Inflow NFT", "CIF");

        // Deploy Pool NFTs

        // deploy pool member NFT contract
        UUPSProxy poolMemberProxy = new UUPSProxy();

        // deploy pool admin NFT contract
        UUPSProxy poolAdminProxy = new UUPSProxy();

        // we deploy mock NFT contracts for the tests to access internal functions
        poolMemberNFTLogic = new PoolMemberNFTMock(sf.host);
        poolAdminNFTLogic = new PoolAdminNFTMock(sf.host);

        poolMemberNFTLogic.castrate();
        poolAdminNFTLogic.castrate();

        // initialize proxy to point at logic
        poolMemberProxy.initializeProxy(address(poolMemberNFTLogic));

        // initialize proxy to point at logic
        poolAdminProxy.initializeProxy(address(poolAdminNFTLogic));

        poolMemberNFT = PoolMemberNFTMock(address(poolMemberProxy));
        poolAdminNFT = PoolAdminNFTMock(address(poolAdminProxy));

        // Deploy TestToken
        TestToken testTokenMock = new TestToken(
            "Mock Test",
            "MT",
            18,
            100000000
        );

        // Deploy SuperToken proxy
        UUPSProxy superTokenMockProxy = new UUPSProxy();

        // deploy super token mock for testing with mock constant outflow/inflow NFTs
        SuperTokenMock superTokenMockLogic = new SuperTokenMock(
            sf.host,
            0,
            IConstantOutflowNFT(address(constantOutflowNFT)),
            IConstantInflowNFT(address(constantInflowNFT)),
            IPoolAdminNFT(address(poolMemberNFT)),
            IPoolMemberNFT(address(poolAdminNFT))
        );
        superTokenMockProxy.initializeProxy(address(superTokenMockLogic));

        superTokenMock = SuperTokenMock(address(superTokenMockProxy));
        superTokenMock.initialize(testTokenMock, 18, "Super Mock Test", "MTx");

        // mint tokens to test accounts
        for (uint256 i = 0; i < N_TESTERS; i++) {
            superTokenMock.mintInternal(TEST_ACCOUNTS[i], INIT_SUPER_TOKEN_BALANCE, "0x", "0x");
        }

        vm.prank(sf.governance.owner());
    }

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
}
