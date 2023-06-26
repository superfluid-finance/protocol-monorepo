// -*- mode: c++; eval: (flycheck-mode -1); -*-

using CertoraSuperfluidPool1 as p1
using CertoraSuperfluidPool2 as p2


methods {
    //operatorSetIndex((int128 tu, (uint32 ts,int128 fr,int256 sv))) returns (bool)
    //    => p1_set_index(tu, ts, fr, sv);
    getIndex() returns PDPoolIndex
        => p1_get_index();
    // p1.getClaimable(uint32 t, address m) returns (int256) => p1_get_claimable();
    // p1.getMemberFlowRate(address m) returns (int128) => get_member_flow_rate();
}

ghost int128 p1_idx_tu;
ghost uint32 p1_idx_ts;
ghost int128 p1_idx_fr;
ghost int256 p1_idx_sv;

function p1_set_index(int128 tu, uint32 ts, int128 fr, int256 sv) returns bool {
    havoc p1_idx_tu assuming (tu == p1_idx_tu@new);
    havoc p1_idx_ts assuming (ts == p1_idx_ts@new);
    havoc p1_idx_fr assuming (fr == p1_idx_fr@new);
    havoc p1_idx_sv assuming (sv == p1_idx_sv@new);
    return true;
}

function p1_get_index() returns int128, uint32, int128, int256 {
    return (p1_idx_tu, (p1_idx_ts, p1_idx_fr, p1_idx_sv));
}
