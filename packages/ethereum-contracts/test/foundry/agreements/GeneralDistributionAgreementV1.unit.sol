// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    IBeacon
} from "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import "../FoundrySuperfluidTester.sol";
import {
    GeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    ISuperfluidToken
} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import {
    ISuperTokenPool,
    SuperTokenPool
} from "../../../contracts/superfluid/SuperTokenPool.sol";
import "forge-std/Test.sol";

contract GeneralDistributionAgreementV1UnitTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(2) {}

    SuperTokenPool public pool;
    uint256 public liquidationPeriod;

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = SuperTokenPool(address(sf.gda.createPool(alice, superToken)));
        (liquidationPeriod, ) = sf.governance.getPPPConfig(sf.host, superToken);
    }

    function test_Set_Get_UIndex(
        address owner,
        uint32 settledAt,
        int96 flowRate,
        int256 settledValue
    ) public {
        bytes memory eff = abi.encode(superToken);
        BasicParticle memory p = BasicParticle({
            _settled_at: Time.wrap(settledAt),
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_value: Value.wrap(settledValue)
        });
        sf.gda.setUIndex(eff, owner, p);
        BasicParticle memory setP = sf.gda.getUIndex(eff, owner);

        assertEq(
            Time.unwrap(p._settled_at),
            Time.unwrap(setP._settled_at),
            "settledAt not equal"
        );
        assertEq(
            FlowRate.unwrap(p._flow_rate),
            FlowRate.unwrap(setP._flow_rate),
            "flowRate not equal"
        );
        assertEq(
            Value.unwrap(p._settled_value),
            Value.unwrap(setP._settled_value),
            "settledValue not equal"
        );
    }

    function test_Set_Get_UIndexData(
        address owner,
        uint32 settledAt,
        int96 flowRate,
        int256 settledValue
    ) public {
        bytes memory eff = abi.encode(superToken);
        BasicParticle memory p = BasicParticle({
            _settled_at: Time.wrap(settledAt),
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_value: Value.wrap(settledValue)
        });
        sf.gda.setUIndex(eff, owner, p);
        GeneralDistributionAgreementV1.UniversalIndexData
            memory setUIndexData = sf.gda.getUIndexData(eff, owner);

        assertEq(settledAt, setUIndexData.settledAt, "settledAt not equal");
        assertEq(flowRate, setUIndexData.flowRate, "flowRate not equal");
        assertEq(
            settledValue,
            setUIndexData.settledValue,
            "settledValue not equal"
        );
        assertEq(0, setUIndexData.totalBuffer, "totalBuffer not equal");
        assertEq(false, setUIndexData.isPool, "isPool not equal");
    }

    function test_Set_Get_FlowDistributionData(
        address from,
        address to,
        uint32 newFlowRate,
        uint96 newFlowRateDelta
    ) public {
        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 lastUpdated = block.timestamp;
        sf.gda.setFlowInfo(
            abi.encode(superToken),
            flowHash,
            from,
            to,
            FlowRate.wrap(int128(uint128(newFlowRate))),
            FlowRate.wrap(int128(uint128(newFlowRateDelta)))
        );

        vm.warp(1000);

        (
            bool exist,
            GeneralDistributionAgreementV1.FlowDistributionData
                memory setFlowDistributionData
        ) = sf.gda.getFlowDistributionData(superToken, flowHash);

        assertEq(true, exist, "flow distribution data does not exist");

        assertEq(
            int96(uint96(newFlowRate)),
            setFlowDistributionData.flowRate,
            "flowRate not equal"
        );

        assertEq(
            lastUpdated,
            setFlowDistributionData.lastUpdated,
            "lastUpdated not equal"
        );

        assertEq(0, setFlowDistributionData.buffer, "buffer not equal");
    }

    // Precompile: ECRECOVER breaks this?
    // function test_Set_Get_PDPIndex(
    //     address owner,
    //     int128 totalUnits,
    //     uint32 wrappedSettledAt,
    //     int96 wrappedFlowRate,
    //     int256 wrappedSettledValue
    // ) public {
    //     vm.assume(owner != address(0));
    //     bytes memory eff = abi.encode(superToken);
    //     PDPoolIndex memory pdpIndex = PDPoolIndex({
    //         total_units: Unit.wrap(totalUnits),
    //         _wrapped_particle: BasicParticle({
    //             _settled_at: Time.wrap(wrappedSettledAt),
    //             _flow_rate: FlowRate.wrap(wrappedFlowRate),
    //             _settled_value: Value.wrap(wrappedSettledValue)
    //         })
    //     });
    //     sf.gda.setPDPIndex(eff, owner, pdpIndex);
    //     PDPoolIndex memory setPdpIndex = sf.gda.getPDPIndex(eff, owner);

    //     assertEq(
    //         Unit.unwrap(pdpIndex.total_units),
    //         Unit.unwrap(setPdpIndex.total_units),
    //         "total units not equal"
    //     );
    //     assertEq(
    //         Time.unwrap(pdpIndex._wrapped_particle._settled_at),
    //         Time.unwrap(setPdpIndex._wrapped_particle._settled_at),
    //         "settled at not equal"
    //     );
    //     assertEq(
    //         FlowRate.unwrap(pdpIndex._wrapped_particle._flow_rate),
    //         FlowRate.unwrap(setPdpIndex._wrapped_particle._flow_rate),
    //         "flow rate not equal"
    //     );
    //     assertEq(
    //         Value.unwrap(pdpIndex._wrapped_particle._settled_value),
    //         Value.unwrap(setPdpIndex._wrapped_particle._settled_value),
    //         "settled value not equal"
    //     );
    // }

    function test_Revert_Reinitialize_GDA(IBeacon beacon) public {
        vm.expectRevert("Initializable: contract is already initialized");
        sf.gda.initialize(beacon);
    }

    function test_Adjust_Buffer_Updates_Flow_Distribution_Data(
        address from,
        address to,
        int32 oldFlowRate,
        int32 newFlowRate
    ) public {
        vm.assume(newFlowRate >= 0);

        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 expectedBuffer = uint256(int256(newFlowRate)) *
            liquidationPeriod;
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            from,
            flowHash,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        (
            bool exist,
            GeneralDistributionAgreementV1.FlowDistributionData
                memory flowDistributionData
        ) = sf.gda.getFlowDistributionData(superToken, flowHash);
        assertEq(exist, true, "flow distribution data does not exist");
        assertEq(
            flowDistributionData.buffer,
            expectedBuffer,
            "buffer not equal"
        );
        assertEq(
            flowDistributionData.flowRate,
            int96(newFlowRate),
            "buffer not equal"
        );
    }

    function test_Adjust_Buffer_Updates_Univeral_Index_Data(
        address from,
        address to,
        int32 oldFlowRate,
        int32 newFlowRate
    ) public {
        vm.assume(newFlowRate >= 0);

        bytes32 flowHash = sf.gda.getFlowDistributionId(from, to);
        uint256 bufferDelta = uint256(int256(newFlowRate)) * liquidationPeriod; // expected buffer == buffer delta because of fresh state
        GeneralDistributionAgreementV1.UniversalIndexData
            memory fromUindexDataBefore = sf.gda.getUIndexData(
                abi.encode(superToken),
                from
            );
        GeneralDistributionAgreementV1.UniversalIndexData
            memory gdaUindexDataBefore = sf.gda.getUIndexData(
                abi.encode(superToken),
                address(sf.gda)
            );
        sf.gda.adjustBuffer(
            abi.encode(superToken),
            from,
            flowHash,
            FlowRate.wrap(int128(oldFlowRate)),
            FlowRate.wrap(int128(newFlowRate))
        );

        GeneralDistributionAgreementV1.UniversalIndexData
            memory fromUindexDataAfter = sf.gda.getUIndexData(
                abi.encode(superToken),
                from
            );

        assertEq(
            fromUindexDataBefore.totalBuffer + bufferDelta,
            fromUindexDataAfter.totalBuffer,
            "from total buffer not equal"
        );
        assertEq(
            fromUindexDataBefore.settledValue - int256(bufferDelta),
            fromUindexDataAfter.settledValue,
            "from settled value not shifted to gda"
        );

        GeneralDistributionAgreementV1.UniversalIndexData
            memory gdaUindexDataAfter = sf.gda.getUIndexData(
                abi.encode(superToken),
                address(sf.gda)
            );
        assertEq(
            gdaUindexDataBefore.settledValue + int256(bufferDelta),
            gdaUindexDataAfter.settledValue,
            "gda settled value not shifted from 'from'"
        );
    }
}
