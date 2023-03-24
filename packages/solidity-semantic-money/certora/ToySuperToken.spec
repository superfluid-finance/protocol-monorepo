//  -*- c++ -*-
// Local variables:
// flycheck-mode: nil
// end:

methods {
    pools(address p) returns (bool) envfree;
    isMemberConnected(address p, address m) returns (bool) envfree;
    getNumConnections(address a) returns (uint) envfree;

    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    getNetFlowRate(address) returns (int128) envfree;
    getFlowRate(address, address, uint32) returns (int128) envfree;

    flow(uint32, address a, address b, uint32 i, int128 r) returns (bool);

    getPendingDistribution() returns (int256) => DISPATCHER(true);
    getClaimable(uint32, address) returns (int256) => DISPATCHER(true);
    operatorSetIndex((int128,(uint32,int256,int128))) returns (bool) => DISPATCHER(true);
    operatorConnectMember(uint32,address,bool) returns (bool) => DISPATCHER(true);
}

ghost gh_totalFlowRate() returns int128 {
    init_state axiom gh_totalFlowRate() == 0;
}
hook Sstore uIndexes[KEY address a].(offset 64) int128 r1 (int128 r0) STORAGE {
    havoc gh_totalFlowRate assuming
        to_mathint(gh_totalFlowRate@new()) ==
        to_mathint(gh_totalFlowRate@old()) + (to_mathint(r1) - to_mathint(r0));
}

invariant zero_net_flow_rate() gh_totalFlowRate() == 0
    filtered { f ->
               f.selector != 0x57587b39 && // absorbParticlesFromPool(address[],(uint32,int256,int128)[])
               f.selector != distribute(address,address,int256).selector &&
               f.selector != distributeFlow(address,address,uint32,int128).selector
             }

//function valid_pool_state(address a) returns bool {
//}
//to_mathint(realtimeBalanceAt(a, t)) + to_mathint(realtimeBalanceAt(b, t)) == 0
//{ preserved with (env e) { require pools(e.msg.sender) == false; }}

// rule check_poolless_1to1_flow() {
//     env e1;
//     address a = e1.msg.sender;
//     address b;
//     uint32 i; // flowId
//     int128 r; // new flowRate
//     uint32 t1; require t1 == e1.block.timestamp;
//     uint32 dt2; uint32 t2 = t1 + dt2;

//     // setup basic assumptions
//     require forall address a. pools(a) == false;
//     require pools(a) == false;
//     require pools(b) == false;
//     require getNumConnections(a) == 0;
//     require getNumConnections(b) == 0;

//     int128 ar1 = getNetFlowRate(a);
//     int128 br1 = getNetFlowRate(b);
//     int256 av1 = realtimeBalanceAt(a, t1);
//     int256 bv1 = realtimeBalanceAt(b, t1);

//     // more assumptions to avoid large-value related (acceptable?) violates
//     require av1 <  2^128;
//     require to_mathint(av1) > to_mathint(-(2^128));
//     require bv1 <  2^128;
//     require to_mathint(bv1) > to_mathint(-(2^128));
//     require to_mathint(ar1) + to_mathint(br1) + to_mathint(r) > -(2^96);
//     require to_mathint(ar1) + to_mathint(br1) + to_mathint(r) <  2^96;
//     require to_mathint(dt2) < 2^32;

//     // inductive invariance
//     requireInvariant zero_net_flow_rate(ar1, br1);

//     bool successful = flow(e1, a, b, i, r);
//     assert successful;

//     // validate the correct semantics of flow
//     int256 av2 = realtimeBalanceAt(a, t2);
//     int256 bv2 = realtimeBalanceAt(b, t2);

//     assert to_mathint(av1) + to_mathint(bv1)
//            + (to_mathint(ar1) + to_mathint(br1)) * to_mathint(dt2)
//         == to_mathint(av2) + to_mathint(bv2);
//     //assert gh_totalFlowRate() == 0;
// }

// rule check_buffer_return()

// rule check_buffer_return()
