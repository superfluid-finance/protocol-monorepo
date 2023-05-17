// -*- mode: c++; eval: (flycheck-mode -1); -*-

import "MonetaryTypes.ghosts.spec"
import "ToySuperToken.methods.spec"
import "ToySuperTokenPool.ghosts.spec"

using CertoraSuperfluidPool1 as p1
using CertoraSuperfluidPool2 as p1

ghost totalFlowRate() returns int128 {
    init_state axiom totalFlowRate() == 0;
}

ghost mapping(address => int256) accountFlowRates {
    init_state axiom forall address owner. accountFlowRates[owner] == 0;
}
hook Sload int128 r uIndexes[KEY address owner]._flow_rate STORAGE {
    require to_mathint(accountFlowRates[owner]) == to_mathint(r);
}
hook Sstore uIndexes[KEY address owner]._flow_rate int128 r1 (int128 r0) STORAGE {
    havoc totalFlowRate assuming
        to_mathint(totalFlowRate@new()) ==
        to_mathint(totalFlowRate@old()) + (to_mathint(r1) - to_mathint(r0));
}

ghost pool1_UnitFlowRates() returns int128 {
    init_state axiom pool1_UnitFlowRates() == 0;
}
hook Sload int128 ur p1._pdpIndex._wrapped_particle._flow_rate STORAGE {
    require pool1_UnitFlowRates() == ur;
}
hook Sstore p1._pdpIndex.(offset 96) int128 ur1 (int128 ur0) STORAGE {
    havoc pool1_UnitFlowRates assuming to_mathint(pool1_UnitFlowRates@new()) == to_mathint(ur1);
    havoc totalFlowRate assuming to_mathint(totalFlowRate@new()) ==
        to_mathint(totalFlowRate@old())
        + (to_mathint(pool1_TotalUnits()) * to_mathint(ur1) - to_mathint(pool1_TotalUnits()) * to_mathint(ur0));
}

ghost pool1_TotalUnits() returns int128 {
    init_state axiom pool1_TotalUnits() == 0;
}
hook Sload int128 u p1._pdpIndex.(offset 0) STORAGE {
    require pool1_TotalUnits() == u;
}
hook Sstore p1._pdpIndex.(offset 0) int128 u1 (int128 u0) STORAGE {
    havoc pool1_TotalUnits assuming to_mathint(pool1_TotalUnits@new()) == to_mathint(u1);
    havoc totalFlowRate assuming to_mathint(totalFlowRate@new()) ==
        to_mathint(totalFlowRate@old())
        + (to_mathint(pool1_UnitFlowRates()) * to_mathint(u1) - to_mathint(pool1_UnitFlowRates()) * to_mathint(u0));
}

invariant zero_net_flow_rate() totalFlowRate() == 0
    filtered { f -> f.selector != 0x869abbb3 /* absorbParticlesFromPool(address[],(uint32,int128,int256)[]) */
               && f.selector != operatorSetIndex((int128,(uint32,int256,int128))).selector
               && f.selector != operatorConnectMember(uint32,address,bool).selector }
