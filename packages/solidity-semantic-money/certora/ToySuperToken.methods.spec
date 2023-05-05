// -*- mode: c++; eval: (flycheck-mode -1); -*-

methods {
    // is ISuperToken
    isPool(address p) returns (bool) envfree;
    getNumConnections(address a) returns (uint) envfree;

    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    getNetFlowRate(address) returns (int128) envfree;
    getFlowRate(address, address, uint32) returns (int128) envfree;

    //transfer(address to, uint256 amount) returns (bool);
    //flow(address from, address to, uint32 i, int128 r) returns (bool);

    // is ISuperTokenPoolAdmin
    isMemberConnected(address p, address m) returns (bool) envfree;
    // absorbParticlesFromPool(address[],(uint32,int256,int128)[]) returns (bool);// => DISPATCHER(true);
}
