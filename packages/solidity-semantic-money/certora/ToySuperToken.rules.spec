// -*- mode: c++; eval: (flycheck-mode -1); -*-

import "MonetaryTypes.ghosts.spec"
import "ToySuperToken.methods.spec"
//import "SuperfluidPool.ghosts.spec"


function require_poolless(address a) {
    require isPool(a) == false;
    require getNumConnections(a) <= 0;
}

rule poolless_shift_balance_moves() {
    env e1;
    address a = e1.msg.sender;
    address b;
    int256 x; // shift amount
    uint32 t1; require t1 == e1.block.timestamp;

    require a != b;
    require_poolless(a);
    require_poolless(b);

    int256 a1 = realtimeBalanceAt(a, t1);

    bool successful = shift(e1, a, b, x);
    assert successful;

    int256 a2 = realtimeBalanceAt(a, t1);

    assert to_mathint(a1) - to_mathint(a2) == to_mathint(x);
}

rule poolless_shift_constant_balance_sum() {
    env e1;
    address a = e1.msg.sender;
    address b;
    int256 x; // shift amount
    uint32 t1; require t1 == e1.block.timestamp;

    require_poolless(a);
    require_poolless(b);

    int256 a1 = realtimeBalanceAt(a, t1);
    int256 b1 = realtimeBalanceAt(b, t1);

    bool successful = shift(e1, a, b, x);
    assert successful;

    int256 a2 = realtimeBalanceAt(a, t1);
    int256 b2 = realtimeBalanceAt(b, t1);

    assert to_mathint(a1) - to_mathint(a2) == to_mathint(b2) - to_mathint(b1);
}

rule poolless_balance_move_at_constant_flow_rate() {
    address a;
    uint32 t1;
    uint32 dt2; uint32 t2 = t1 + dt2;

    require_poolless(a);

    int128 r = getNetFlowRate(a);
    int256 v1 = realtimeBalanceAt(a, t1);
    int256 v2 = realtimeBalanceAt(a, t2);

    assert to_mathint(v2) - to_mathint(v1)
        == to_mathint(r) * to_mathint(dt2);
}

rule poolless_flow_constant_flowrate_sum() {
    env e1;
    address a = e1.msg.sender;
    address b;
    uint32 i; // flowId
    int128 r; // new flowRate
    uint32 t1; require t1 == e1.block.timestamp;
    uint32 dt2; uint32 t2 = t1 + dt2;

    require_poolless(a);
    require_poolless(b);

    int128 r0 = getFlowRate(a, b, i);
    int128 ar1 = getNetFlowRate(a);
    int128 br1 = getNetFlowRate(b);

    bool successful = flow(e1, a, b, i, r);
    assert successful;

    int128 r1 = getFlowRate(a, b, i);
    int128 ar2 = getNetFlowRate(a);
    int128 br2 = getNetFlowRate(b);

    assert r == r1;
    assert to_mathint(ar1) - to_mathint(ar2)
        == to_mathint(r) - to_mathint(r0);
    assert to_mathint(ar2) - to_mathint(ar1)
        == to_mathint(br1) - to_mathint(br2);
}

// rule poolless_1to1_flow_constant_balance_sum() {
//     env e1;
//     address a = e1.msg.sender;
//     address b;
//     uint32 i; // flowId
//     int128 r; // new flowRate
//     uint32 t1; require t1 == e1.block.timestamp;
//     uint32 dt2; uint32 t2 = t1 + dt2;

//     require_poolless(a, b);

//     int256 av2a = realtimeBalanceAt(a, t2);
//     int256 bv2a = realtimeBalanceAt(b, t2);

//     bool successful = flow(e1, a, b, i, r);
//     assert successful;

//     int256 av2 = realtimeBalanceAt(a, t2);
//     int256 bv2 = realtimeBalanceAt(b, t2);

//     assert to_mathint(av2) + to_mathint(bv2)
//         == to_mathint(av2a) + to_mathint(bv2a);
// }

// rule poolless_1to1_flow_balance_move_at_constant_rate() {
//     env e1;
//     address a = e1.msg.sender;
//     address b;
//     uint32 i; // flowId
//     int128 r; // new flowRate
//     uint32 t1; require t1 == e1.block.timestamp;
//     uint32 dt2; uint32 t2 = t1 + dt2;

//     require_poolless(a, b);

//     int128 r0 = getFlowRate(a, b, i);
//     int256 av1 = realtimeBalanceAt(a, t1);
//     int256 av2a = realtimeBalanceAt(a, t2);

//     bool successful = flow(e1, a, b, i, r);
//     assert successful;

//     int256 av2 = realtimeBalanceAt(a, t2);

//     assert to_mathint(av2a) - to_mathint(av1)
//         == to_mathint(av2) - to_mathint(av1)
//         + (to_mathint(r) - to_mathint(r0)) * to_mathint(dt2);
// }

// saved some old code snippets:
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

    // assert to_mathint(av1) + to_mathint(bv1)
    //        + (to_mathint(ar1) + to_mathint(br1)) * to_mathint(dt2)
    //     == to_mathint(av2) + to_mathint(bv2);
