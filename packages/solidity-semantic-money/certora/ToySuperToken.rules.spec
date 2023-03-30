// -*- mode: c++; eval: (flycheck-mode -1); -*-

import "ToySuperToken.methods.spec"

rule check_poolless_1to1_flow() {
    env e1;
    address a = e1.msg.sender;
    address b;
    uint32 i; // flowId
    int128 r; // new flowRate
    uint32 t1; require t1 == e1.block.timestamp;
    uint32 dt2; uint32 t2 = t1 + dt2;

    // setup basic assumptions
    require isPool(a) == false;
    require isPool(b) == false;

    int128 ar1 = getNetFlowRate(a);
    int128 br1 = getNetFlowRate(b);
    int256 av1 = realtimeBalanceAt(a, t1);
    int256 bv1 = realtimeBalanceAt(b, t1);

    // more assumptions to avoid large-value related (acceptable?) violates
    // require to_mathint(av1) > to_mathint(-(2^128))
    //     && to_mathint(av1) <  to_mathint(2^128);
    // require to_mathint(bv1) > to_mathint(-(2^128))
    //     && to_mathint(bv1) <  to_mathint(2^128);
    // require to_mathint(ar1) + to_mathint(br1) + to_mathint(r) > -(2^96)
    //     && to_mathint(ar1) + to_mathint(br1) + to_mathint(r) <  2^96;
    // require to_mathint(dt2) < 2^32;

    // inductive invariance
    // requireInvariant zero_net_flow_rate(ar1, br1);

    bool successful = flow(e1, a, b, i, r);
    assert successful;

    // validate the correct semantics of flow
    int256 av2 = realtimeBalanceAt(a, t2);
    int256 bv2 = realtimeBalanceAt(b, t2);

    assert to_mathint(av1) + to_mathint(bv1)
           + (to_mathint(ar1) + to_mathint(br1)) * to_mathint(dt2)
        == to_mathint(av2) + to_mathint(bv2);
    //assert gh_totalFlowRate() == 0;
}
