methods {
    pools(address) returns (bool) envfree;
    getNumConnections(address) returns (uint) envfree;

    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    getNetFlowRate(address) returns (int128) envfree;
    getFlowRate(address, address, uint32) returns (int128) envfree;

    flow(uint32, address, address, uint32, int128) returns (bool);

    getPendingDistribution() returns (int256) => DISPATCHER(true);
    getClaimable(uint32, address) returns (int256) => DISPATCHER(true);
}

rule check_1to1_flow() {
    env e1;
    address a = e1.msg.sender; address b;
    uint32 i; int128 r;
    uint32 t1; require t1 == e1.block.timestamp;
    uint32 dt2; uint32 t2 = t1 + dt2;

    // setup assumptions
    require (forall address a. pools(a) == false);
    require (forall address a. getNumConnections(a) == 0); // TODO this should be implied by above line as an invariance?
    require getFlowRate(a, b, i) == 0;

    int128 tr = getNetFlowRate(a) + getNetFlowRate(b);
    int256 a1 = realtimeBalanceAt(a, t1);
    int256 b1 = realtimeBalanceAt(b, t1);

    bool successful = flow(e1, a, b, i, r);
    assert successful;

    int256 a2 = realtimeBalanceAt(a, t2);
    int256 b2 = realtimeBalanceAt(b, t2);

    assert to_mathint(a1) + to_mathint(b1)
        == to_mathint(a2) + to_mathint(b2) + to_mathint(tr) * to_mathint(dt2);
}
