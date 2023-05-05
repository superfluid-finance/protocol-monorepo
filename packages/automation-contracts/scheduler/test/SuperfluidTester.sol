// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { Test } from "forge-std/Test.sol";
import { IERC1820Registry } from "@openzeppelin/contracts/utils/introspection/IERC1820Registry.sol";
import { ERC1820RegistryCompiled } from "@superfluid-finance/ethereum-contracts/contracts/libs/ERC1820RegistryCompiled.sol";
import { SuperfluidFrameworkDeployer } from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeployer.sol";
import {
    Superfluid,
    ConstantFlowAgreementV1,
    InstantDistributionAgreementV1,
    SuperTokenFactory
} from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol";
import { SuperToken, SuperTokenDeployer, TestToken } from "@superfluid-finance/ethereum-contracts/contracts/utils/SuperTokenDeployer.sol";
import { CFAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/CFAv1Library.sol";
import { IDAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";
import { FlowScheduler } from "./../contracts/FlowScheduler.sol";

// Helper for foundry tests of Superfluid related contracts
contract SuperfluidTester is Test {

    uint256 internal constant INIT_TOKEN_BALANCE = type(uint128).max;
    uint256 internal constant INIT_SUPER_TOKEN_BALANCE = type(uint64).max;
    address internal constant admin = address(0x420);

    address internal constant alice = address(0x421);
    address internal constant bob = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant eve = address(0x424);

    address[] internal TEST_ACCOUNTS = [admin, alice, bob, carol, eve];
    uint256 internal immutable N_TESTERS;

    TestToken internal token;
    SuperToken internal superToken;

    uint256 internal _expectedTotalSupply = 0;

    SuperfluidFrameworkDeployer internal immutable sfDeployer;
    SuperTokenDeployer internal immutable superTokenDeployer;
    SuperfluidFrameworkDeployer.Framework internal sf;
    Superfluid host;
    ConstantFlowAgreementV1 cfa;
    FlowScheduler internal flowScheduler;

    constructor(uint8 nTesters) {
        require(nTesters <= TEST_ACCOUNTS.length, "too many testers");
        N_TESTERS = nTesters;

        vm.startPrank(admin);
        vm.etch(ERC1820RegistryCompiled.at, ERC1820RegistryCompiled.bin);
        sfDeployer = new SuperfluidFrameworkDeployer();
        sf = sfDeployer.getFramework();

        // deploy SuperTokenDeployer
        superTokenDeployer = new SuperTokenDeployer(
            address(sf.superTokenFactory),
            address(sf.resolver)
        );

        // transfer ownership of TestGovernance to superTokenDeployer
        // governance ownership is required for initializing the NFT
        // contracts on the SuperToken
        sfDeployer.transferOwnership(address(superTokenDeployer));

        // add superTokenDeployer as admin to the resolver so it can register the SuperTokens
        sf.resolver.addAdmin(address(superTokenDeployer));

        host = sf.host;
        cfa = sf.cfa;
        vm.stopPrank();

        /// @dev Example Flow Scheduler to test
        flowScheduler = new FlowScheduler(host, "");
    }
}
