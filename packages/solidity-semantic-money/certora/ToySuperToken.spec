//  -*- c++ -*-
// Local variables:
// flycheck-mode: nil
// end:

import "Setup.spec"

using ToySuperTokenPoolCertora as p1

methods {
    isPool(address p) returns (bool) envfree;
    isMemberConnected(address p, address m) returns (bool) envfree;
    getNumConnections(address a) returns (uint) envfree;

    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    getNetFlowRate(address) returns (int128) envfree;
    getFlowRate(address, address, uint32) returns (int128) envfree;

    flow(uint32, address a, address b, uint32 i, int128 r) returns (bool);

    getIndex() returns ((int128,(uint32,int256,int128))) => DISPATCHER(true);
    getDistributionFlowRate() returns (int128) => DISPATCHER(true);
    operatorSetIndex((int128,(uint32,int256,int128))) returns (bool) => DISPATCHER(true);
    getPendingDistribution() returns (int256) => DISPATCHER(true);
    getClaimable(uint32, address) returns (int256) => DISPATCHER(true);
    operatorConnectMember(uint32,address,bool) returns (bool) => DISPATCHER(true);
    absorbParticlesFromPool(address[],(uint32,int256,int128)[]) returns (bool) => DISPATCHER(true);
}

ghost totalFlowRate() returns int128 {
    init_state axiom totalFlowRate() == 0;
}

ghost mapping(address => int256) accountFlowRates {
    init_state axiom forall address owner. accountFlowRates[owner] == 0;
}
hook Sload int128 r uIndexes[KEY address owner].flow_rate STORAGE {
    require to_mathint(accountFlowRates[owner]) == to_mathint(r);
}
hook Sstore uIndexes[KEY address owner].flow_rate int128 r1 (int128 r0) STORAGE {
    havoc totalFlowRate assuming
        to_mathint(totalFlowRate@new()) ==
        to_mathint(totalFlowRate@old()) + (to_mathint(r1) - to_mathint(r0));
}

ghost pool1_UnitFlowRates() returns int128 {
    init_state axiom pool1_UnitFlowRates() == 0;
}
hook Sload int128 ur p1._pdpIndex.(offset 96) STORAGE {
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
    filtered { f -> f.selector != 0x57587b39 /* absorbParticlesFromPool(address[],(uint32,int256,int128)[]) */
               && f.selector != operatorSetIndex((int128,(uint32,int256,int128))).selector
               && f.selector != operatorConnectMember(uint32,address,bool).selector }

//{ preserved { require forall address a. (a == p1 && isPool(a) == true) || isPool(a) == false; } }

// rule check_poolless_1to1_flow() {
//      env e1;
//      address a = e1.msg.sender;
//      address b;
//      uint32 i; // flowId
//      int128 r; // new flowRate
//      uint32 t1; require t1 == e1.block.timestamp;
//      uint32 dt2; uint32 t2 = t1 + dt2;

//      // setup basic assumptions
//      require forall address a. isPool(a) == false;
//      require isPool(a) == false;
//      require isPool(b) == false;
//      require getNumConnections(a) == 0;
//      require getNumConnections(b) == 0;

//      int128 ar1 = getNetFlowRate(a);
//      int128 br1 = getNetFlowRate(b);
//      int256 av1 = realtimeBalanceAt(a, t1);
//      int256 bv1 = realtimeBalanceAt(b, t1);

//      // more assumptions to avoid large-value related (acceptable?) violates
//      require av1 <  2^128;
//      require to_mathint(av1) > to_mathint(-(2^128));
//      require bv1 <  2^128;
//      require to_mathint(bv1) > to_mathint(-(2^128));
//      require to_mathint(ar1) + to_mathint(br1) + to_mathint(r) > -(2^96);
//      require to_mathint(ar1) + to_mathint(br1) + to_mathint(r) <  2^96;
//      require to_mathint(dt2) < 2^32;

//      // inductive invariance
//      // requireInvariant zero_net_flow_rate(ar1, br1);

//      bool successful = flow(e1, a, b, i, r);
//      assert successful;

//      // validate the correct semantics of flow
//      int256 av2 = realtimeBalanceAt(a, t2);
//      int256 bv2 = realtimeBalanceAt(b, t2);

//      assert to_mathint(av1) + to_mathint(bv1)
//             + (to_mathint(ar1) + to_mathint(br1)) * to_mathint(dt2)
//          == to_mathint(av2) + to_mathint(bv2);
//      //assert gh_totalFlowRate() == 0;
//  }
