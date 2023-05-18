// -*- mode: c++; eval: (flycheck-mode -1); -*-

methods {
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
