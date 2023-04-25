// -*- mode: c++; eval: (flycheck-mode -1); -*-

methods {
    mul(int256 a, int128 b) returns (int256) => mul_vu_cvl(a, b);
    div(int256 a, int128 b) returns (int256) => div_vu_cvl(a, b);

    mul(int128 a, uint32 b) returns (int256) => mul_ft_cvl(a, b);

    mul(int128 a, int128 b) returns (int128) => mul_fu_cvl(a, b);
    div(int128 a, int128 b) returns (int128) => div_fu_cvl(a, b);
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
