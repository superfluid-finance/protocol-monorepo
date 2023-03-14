methods {
    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    _flow(uint32, address, address, uint32, int128) returns (bool) envfree;
}

rule check_1to1_flow() {
    address a; address b; uint32 i; int128 r; uint16 t1; uint16 t2;

    int256 a1 = realtimeBalanceAt(a, t1);
    int256 b1 = realtimeBalanceAt(b, t1);

    bool successful = _flow(t1, a, b, i, r);
    assert successful;

    // in reality I should restrict t2 > t1, but in math it's not relevant
    int256 a2 = realtimeBalanceAt(a, t2);
    int256 b2 = realtimeBalanceAt(b, t2);

    assert to_mathint(a1) + to_mathint(b1) == to_mathint(a2) + to_mathint(b2);
}
