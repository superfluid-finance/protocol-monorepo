// -*- mode: c++; eval: (flycheck-mode -1); -*-

methods {
    // Monetary Types
    mul(int256 a, int128 b) returns (int256) => mul_vu_cvl(a, b);
    div(int256 a, int128 b) returns (int256) => div_vu_cvl(a, b);

    mul(int128 a, uint32 b) returns (int256) => mul_ft_cvl(a, b);

    mul(int128 a, int128 b) returns (int128) => mul_fu_cvl(a, b);
    div(int128 a, int128 b) returns (int128) => div_fu_cvl(a, b);
    rem(int128 a, int128 b) returns (int128) => rem_fu_cvl(a, b);

    // ISuperToken
    isPool(address p) returns (bool) envfree;
    getNumConnections(address a) returns (uint) envfree;

    realtimeBalanceAt(address, uint32) returns (int256) envfree;
    getNetFlowRate(address) returns (int128) envfree;
    getFlowRate(address, address, uint32) returns (int128) envfree;

    //transfer(address to, uint256 amount) returns (bool);
    //flow(address from, address to, uint32 i, int128 r) returns (bool);

    // ISuperTokenPoolAdmin
    isMemberConnected(address p, address m) returns (bool) envfree;
    // absorbParticlesFromPool(address[],(uint32,int256,int128)[]) returns (bool);// => DISPATCHER(true);

    // ISuperTokenPool
    getIndex() returns ((int128,(uint32,int256,int128))) => DISPATCHER(true);
    getTotalUnits() returns (int128) => DISPATCHER(true);
    getDistributionFlowRate() returns (int128) => DISPATCHER(true);
    getPendingDistributionFlowRate() returns (int128) => DISPATCHER(true);
    getMemberFlowRate(address) returns (int128) => DISPATCHER(true);
    getPendingDistribution() returns (int256) => DISPATCHER(true);
    getClaimable(uint32, address) returns (int256) => DISPATCHER(true);
    operatorSetIndex((int128,(uint32,int256,int128))) returns (bool) => DISPATCHER(true);
    operatorConnectMember(uint32,address,bool) returns (bool) => DISPATCHER(true);
}

ghost int256 mul_vu;
function mul_vu_cvl(int256 a, int128 b) returns int256 {
    havoc mul_vu assuming (
        to_mathint(mul_vu@new) == to_mathint(a) * to_mathint(b)
    );
    require to_mathint(mul_vu) <= 2^256 - 1 && to_mathint(mul_vu) >= -1*2^256;
    return mul_vu;
}
ghost int256 div_vu;
function div_vu_cvl(int256 a, int128 b) returns int256 {
    havoc div_vu assuming (
        to_mathint(div_vu@new) == to_mathint(a) / to_mathint(b)
    );
    require to_mathint(div_vu) <= 2^256 - 1 && to_mathint(div_vu) >= -1*2^256;
    return div_vu;
}

ghost int256 mul_ft;
function mul_ft_cvl(int128 a, uint32 b) returns int256 {
    havoc mul_ft assuming (
        to_mathint(mul_ft@new) == to_mathint(a) * to_mathint(b)
    );
    require to_mathint(mul_ft) <= 2^256 - 1 && to_mathint(mul_ft) >= -1*2^256;
    return mul_ft;
}

ghost int128 mul_fu;
function mul_fu_cvl(int128 a, int128 b) returns int128 {
    havoc mul_fu assuming (
        to_mathint(mul_fu@new) == to_mathint(a) * to_mathint(b)
    );
    require to_mathint(mul_fu) <= 2^128 - 1 && to_mathint(mul_fu) >= -1*2^128;
    return mul_fu;
}
ghost int128 div_fu;
function div_fu_cvl(int128 a, int128 b) returns int128 {
    havoc div_fu assuming (
        to_mathint(div_fu@new) == to_mathint(a) / to_mathint(b)
    );
    require to_mathint(div_fu) <= 2^128 - 1 && to_mathint(div_fu) >= -1*2^128;
    return div_fu;
}
ghost int128 rem_fu;
function rem_fu_cvl(int128 a, int128 b) returns int128 {
    havoc rem_fu assuming (
        to_mathint(rem_fu@new) == to_mathint(a) % to_mathint(b)
    );
    require to_mathint(rem_fu) <= 2^128 - 1 && to_mathint(rem_fu) >= -1*2^128;
    return rem_fu;
}
