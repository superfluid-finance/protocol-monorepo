%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem

from src.utils.SemanticMoney import (
    SemanticMoney,
    BasicParticle,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
)

@external
func test_m_append{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    // If `_settled_at` of b of greater
    let a = BasicParticle(5, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle._flow_rate = 20;
    assert newBasicParticle._settled_at = 10;
    assert newBasicParticle._settled_value = 70;

    // if `_settled_at` of a and b are same
    let a = BasicParticle(10, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle._flow_rate = 20;
    assert newBasicParticle._settled_at = 10;
    assert newBasicParticle._settled_value = 20;

    // if `_settled_at` of a of greater
    let a = BasicParticle(20, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle._flow_rate = 20;
    assert newBasicParticle._settled_at = 20;
    assert newBasicParticle._settled_value = 120;

    let (_newBasicParticle) = SemanticMoney.mappend(b, newBasicParticle);
    assert _newBasicParticle._flow_rate = 30;
    assert _newBasicParticle._settled_at = 20;
    assert _newBasicParticle._settled_value = 230;
    return ();
}

@external
func setup_flowrate_quotrem_unit{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            r = strategy.felts(), 
            u = strategy.integers(0, 1000),
        )
    %}
    return ();
}

@external
func test_flowrate_quotrem_unit{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(r: felt, u: felt) {
    %{ assume(ids.u != 0) %}
    %{ assume(ids.r <= 3402823669209384634633746074317682114) %}
    let (q, e) = unsigned_div_rem(r, u);
    assert (q * u) + e = r;
    return ();
}

@external
func setup_distributive_law{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            x = strategy.felts(), 
            u1 = strategy.integers(0, 1000),
            u2 = strategy.integers(0, 1000), 
        )
    %}
    return ();
}

@external
func test_distributive_law{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(x: felt, u1: felt, u2: felt) {
    assert (x * u1) + (x * u2) = x * (u1 + u2);
    return ();
}

@external
func setup_u_monoid_identity{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            _settled_at = strategy.integers(0, 1000), 
            _settled_value = strategy.integers(0, 1000),
            _flow_rate = strategy.felts(), 
        )
    %}
    return ();
}

@external
func test_u_monoid_identity{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_settled_at: felt, _settled_value: felt, _flow_rate: felt) {
    alloc_locals;
    let p1 = BasicParticle(_settled_at, _settled_value, _flow_rate);
    let ep = BasicParticle(0, 0, 0);
    let (p2) = SemanticMoney.mappend(ep, p1);
    assert p2 = p1;
    let (p3) = SemanticMoney.mappend(p1, ep);
    assert p3 = p1;
    return ();
}

@external
func setup_u_monoid_assoc{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            _settled_at_1 = strategy.integers(0, 1000), 
            _settled_value_1 = strategy.integers(0, 1000),
            _flow_rate_1 = strategy.felts(),
            _settled_at_2 = strategy.integers(0, 1000), 
            _settled_value_2 = strategy.integers(0, 1000),
            _flow_rate_2 = strategy.felts(), 
            _settled_at_3 = strategy.integers(0, 1000), 
            _settled_value_3 = strategy.integers(0, 1000),
            _flow_rate_3 = strategy.felts(),  
        )
    %}
    return ();
}

@external
func test_u_monoid_assoc{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_settled_at_1: felt, _settled_value_1: felt, _flow_rate_1: felt, _settled_at_2: felt, _settled_value_2: felt, _flow_rate_2: felt, _settled_at_3: felt, _settled_value_3: felt, _flow_rate_3: felt) {
    alloc_locals;
    let p1 = BasicParticle(_settled_at_1, _settled_value_1, _flow_rate_1);
    let p2 = BasicParticle(_settled_at_2, _settled_value_2, _flow_rate_2);
    let p3 = BasicParticle(_settled_at_3, _settled_value_3, _flow_rate_3);
    let (p1_and_p2) = SemanticMoney.mappend(p1, p2);
    let (agg1) = SemanticMoney.mappend(p1_and_p2, p3);
    let (p2_and_p3) = SemanticMoney.mappend(p2, p3);
    let (agg2) = SemanticMoney.mappend(p2_and_p3, p1);
    assert agg1 = agg2;
    return ();
}

@external
func setup_u_settle_idempotence{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            _settled_at = strategy.integers(0, 1000), 
            _settled_value = strategy.integers(0, 1000),
            _flow_rate = strategy.felts(),
            time = strategy.integers(1, 1000),
        )
    %}
    return ();
}

@external
func test_u_settle_idempotence{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_settled_at: felt, _settled_value: felt, _flow_rate: felt, time: felt) {
    alloc_locals;
    let p = BasicParticle(_settled_at, _settled_value, _flow_rate);
    let (settled_at) = SemanticMoney.settled_at(p);
    let t1 = settled_at + time;
    let (p1) = SemanticMoney.settle(p, t1);
    let (p2) = SemanticMoney.settle(p1, t1);
    assert p1._settled_at = t1;
    assert p1 = p2;
    return ();
}

@external
func setup_u_constant_rtb{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            _settled_at = strategy.integers(0, 1000), 
            _settled_value = strategy.integers(0, 1000),
            _flow_rate = strategy.felts(),
            m1 = strategy.integers(1, 1000),
            m2 = strategy.integers(1, 1000),
            m3 = strategy.integers(1, 1000),
        )
    %}
    return ();
}

@external
func test_u_constant_rtb{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_settled_at: felt, _settled_value: felt, _flow_rate: felt, m1: felt, m2: felt, m3: felt) {
    alloc_locals;
    let p = BasicParticle(_settled_at, _settled_value, _flow_rate);
    let (settled_at) = SemanticMoney.settled_at(p);
    let t1 = settled_at + m1;
    let t2 = t1 + m2;
    let t3 = t2 + m3;
    let (settle_p_at_t1) = SemanticMoney.settle(p, t1);
    let (settle_p_at_t2) = SemanticMoney.settle(p, t2);
    let (rtb_of_settle_p_at_t1_at_t3) = SemanticMoney.realtime_balance_of(settle_p_at_t1, t3);
    let (rtb_of_settle_p_at_t2_at_t3) = SemanticMoney.realtime_balance_of(settle_p_at_t2, t3);
    let (rtb_of_p_at_t3) = SemanticMoney.realtime_balance_of(p, t3);
    assert rtb_of_settle_p_at_t1_at_t3 = rtb_of_p_at_t3;
    assert rtb_of_settle_p_at_t2_at_t3 = rtb_of_p_at_t3;
    let (settle_p_at_t1_at_t2) = SemanticMoney.settle(settle_p_at_t1, t2);
    let (rtb_of_settle_p_at_t1_at_t2_at_t3) = SemanticMoney.realtime_balance_of(settle_p_at_t1_at_t2, t3);
    assert rtb_of_settle_p_at_t1_at_t2_at_t3 = rtb_of_p_at_t3;
    return ();
}

@external
func setup_realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.felts(), # flow rate
            sv = strategy.felts(), # settled value
            t1 = strategy.integers(0, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
        )
    %}
    return ();
}

@external
func test_realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, t1: felt, t2: felt, t3: felt, t4: felt
) {
    let u_index = BasicParticle(t1, sv, fr);

    let (balance) = SemanticMoney.realtime_balance_of(u_index, t2);
    assert balance = ((t2 - t1) * fr) + sv;

    let (balance) = SemanticMoney.realtime_balance_of(u_index, t3);
    assert balance = ((t3 - t1) * fr) + sv;

    let (balance) = SemanticMoney.realtime_balance_of(u_index, t4);
    assert balance = ((t4 - t1) * fr) + sv;

    return ();
}

@external
func setup_realtime_balance_of_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            tu = strategy.felts(), # total unit
            ou = strategy.felts(), # owned unit
            fr = strategy.felts(), # flow rate
            sv_wrapped = strategy.felts(), # settled value in wrapped particle
            sv_synced = strategy.felts(), # settled value in synced particle
            sv_for_member = strategy.felts(), # settled value for pd member
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
        )
    %}
    return ();
}

@external
func test_realtime_balance_of_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(
    tu: felt,
    ou: felt,
    fr: felt,
    sv_wrapped: felt,
    sv_synced: felt,
    sv_for_member: felt,
    t1: felt,
    t2: felt,
    t3: felt,
    t4: felt,
) {
    let pdPoolIndex = PDPoolIndex(tu, BasicParticle(t1, sv_wrapped, fr));
    let pdPoolMember = PDPoolMember(ou, sv_for_member, BasicParticle(t1, sv_synced, fr));
    let pdPoolMemberMU = PDPoolMemberMU(pdPoolIndex, pdPoolMember);
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t1, sv_wrapped, fr), t2
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t1, sv_synced, fr), t1
    );
    let (balance) = SemanticMoney.realtime_balance_of_pool_member_mu(pdPoolMemberMU, t2);
    assert balance = ((balance_for_wrapped_particle - balance_for_synced_particle) * ou) +
        sv_for_member;

    let pdPoolIndex = PDPoolIndex(tu, BasicParticle(t2, sv_wrapped, fr));
    let pdPoolMember = PDPoolMember(ou, sv_for_member, BasicParticle(t2, sv_synced, fr));
    let pdPoolMemberMU = PDPoolMemberMU(pdPoolIndex, pdPoolMember);
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t2, sv_wrapped, fr), t3
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t2, sv_synced, fr), t2
    );
    let (balance) = SemanticMoney.realtime_balance_of_pool_member_mu(pdPoolMemberMU, t3);
    assert balance = ((balance_for_wrapped_particle - balance_for_synced_particle) * ou) +
        sv_for_member;

    let pdPoolIndex = PDPoolIndex(tu, BasicParticle(t3, sv_wrapped, fr));
    let pdPoolMember = PDPoolMember(ou, sv_for_member, BasicParticle(t3, sv_synced, fr));
    let pdPoolMemberMU = PDPoolMemberMU(pdPoolIndex, pdPoolMember);
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t3, sv_wrapped, fr), t4
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t3, sv_synced, fr), t3
    );
    let (balance) = SemanticMoney.realtime_balance_of_pool_member_mu(pdPoolMemberMU, t4);
    assert balance = ((balance_for_wrapped_particle - balance_for_synced_particle) * ou) +
        sv_for_member;
    return ();
}

@external
func setup_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.felts(), # flow rate
            sv = strategy.felts(), # settled value
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
        )
    %}
    return ();
}

@external
func test_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, t1: felt, t2: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex) = SemanticMoney.settle(index, t2);
    assert newIndex._settled_value = ((t2 - t1) * fr) + sv;
    assert newIndex._settled_at = t2;
    return ();
}

@external
func setup_settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    ) {
    %{
        given(
            tu = strategy.felts(), # total unit
            ou = strategy.felts(), # owned unit
            fr = strategy.felts(), # flow rate
            sv = strategy.felts(), # settled value in wrapped particle
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
        )
    %}
    return ();
}

@external
func test_settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    tu: felt, ou: felt, fr: felt, sv: felt, t1: felt, t2: felt, t3: felt, t4: felt
) {
    let poolIndex = PDPoolIndex(tu, BasicParticle(t1, sv, fr));
    let (settled_pool_index) = SemanticMoney.settle_for_pool_index(poolIndex, t2);
    assert settled_pool_index._wrapped_particle._settled_at = t2;
    assert settled_pool_index._wrapped_particle._settled_value = ((t2 - t1) * fr) + sv;

    let (_settled_pool_index) = SemanticMoney.settle_for_pool_index(settled_pool_index, t3);
    assert _settled_pool_index._wrapped_particle._settled_at = t3;
    assert _settled_pool_index._wrapped_particle._settled_value = ((t3 - t2) * fr) + (
        ((t2 - t1) * fr) + sv
    );

    let (__settled_pool_index) = SemanticMoney.settle_for_pool_index(_settled_pool_index, t4);
    assert __settled_pool_index._wrapped_particle._settled_at = t4;
    assert __settled_pool_index._wrapped_particle._settled_value = ((t4 - t3) * fr) + (
        (t3 - t2) * fr
    ) + (((t2 - t1) * fr) + sv);
    return ();
}

@external
func setup_settle_for_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            tu = strategy.felts(), # total unit
            ou = strategy.felts(), # owned unit
            fr = strategy.felts(), # flow rate
            sv_wrapped = strategy.felts(), # settled value in wrapped particle
            sv_synced = strategy.felts(), # settled value in synced particle
            sv_for_member = strategy.felts(), # settled value for pd member
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
        )
    %}
    return ();
}

@external
func test_settle_for_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(
    tu: felt,
    ou: felt,
    fr: felt,
    sv_wrapped: felt,
    sv_synced: felt,
    sv_for_member: felt,
    t1: felt,
    t2: felt,
) {
    let poolIndex = PDPoolIndex(tu, BasicParticle(t1, sv_wrapped, fr));
    let poolMember = PDPoolMember(ou, sv_for_member, BasicParticle(t1, sv_synced, fr));
    let poolMemberMU = PDPoolMemberMU(poolIndex, poolMember);
    let (settled_poolMemberMU) = SemanticMoney.settle_for_pool_member_mu(poolMemberMU, t2);

    assert settled_poolMemberMU.pdPoolIndex._wrapped_particle._settled_at = t2;
    assert settled_poolMemberMU.pdPoolIndex._wrapped_particle._settled_value = ((t2 - t1) * fr) +
        sv_wrapped;
    assert settled_poolMemberMU.pdPoolMember._synced_particle._settled_at = settled_poolMemberMU.pdPoolIndex._wrapped_particle._settled_at;
    assert settled_poolMemberMU.pdPoolMember._synced_particle._settled_value = settled_poolMemberMU.pdPoolIndex._wrapped_particle._settled_value;
    let (balanceOfPDMemberMU) = SemanticMoney.realtime_balance_of_pool_member_mu(poolMemberMU, t2);
    assert settled_poolMemberMU.pdPoolMember._settled_value = balanceOfPDMemberMU;
    return ();
}

@external
func setup_shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.felts(), # flow rate
            sv = strategy.felts(), # settled value
            amount = strategy.felts(), 
            t1 = strategy.integers(0, 100), # time 1
        )
    %}
    return ();
}

@external
func test_shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, amount: felt, t1: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex, _amount) = SemanticMoney.shift1(amount, index);
    assert newIndex._settled_value = amount + sv;
    assert _amount = amount;
    return ();
}

@external
func setup_shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.felts(), # flow rate
            amount = strategy.felts(), 
            t1 = strategy.integers(0, 100), # time 1
        )
    %}
    return ();
}

@external
func test_shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, amount: felt, t1: felt
) {
    let aIndex = BasicParticle(0, 0, 0);
    let bIndex = BasicParticle(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.shift2(aIndex, bIndex, amount);
    assert _aIndex._settled_value = (((t1 - _aIndex._settled_at) * _aIndex._flow_rate) + 0) -
        amount;
    assert _bIndex._settled_value = (((t1 - _bIndex._settled_at) * _bIndex._flow_rate) + 0) +
        amount;
    assert _aIndex._settled_value + _bIndex._settled_value = 0;
    return ();
}

@external
func setup_flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.felts(), # flow rate
            sv = strategy.felts(), # settled value
            fr_1 = strategy.felts(), # new flow rate
            t1 = strategy.integers(0, 100), # time 
        )
    %}
    return ();
}

@external
func test_flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, fr_1: felt, t1: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex, _) = SemanticMoney.flow1(fr_1, index);
    assert newIndex._flow_rate = fr_1;
    return ();
}

@external
func setup_flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr_1 = strategy.felts(), # flow rate for sender
            t1 = strategy.integers(0, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
        )
    %}
    return ();
}

@external
func test_flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr_1: felt, t1: felt, t2: felt
) {
    let aIndex = BasicParticle(0, 0, 0);
    let bIndex = BasicParticle(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.flow2(aIndex, bIndex, fr_1, t1);
    assert _aIndex._flow_rate = -fr_1;
    assert _bIndex._flow_rate = fr_1;

    let (balance_for_sender_particle) = SemanticMoney.realtime_balance_of(_aIndex, t2);
    let (balance_for_receiver_particle) = SemanticMoney.realtime_balance_of(_bIndex, t2);
    assert balance_for_sender_particle + balance_for_receiver_particle = 0;
    return ();
}

@external
func setup_shiftFlow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr1 = strategy.felts(), # flow rate
            fr2 = strategy.felts(), # flow rate
            fr3 = strategy.felts(), # flow rate
            sv1 = strategy.felts(), # settled value
            sv2 = strategy.felts(), # settled value
            t1 = strategy.integers(0, 100), # time 
            t2 = strategy.integers(101, 200), # time 
            t3 = strategy.integers(201, 300), # time 
        )
    %}
    return ();
}

@external
func test_shiftFlow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, fr3: felt, sv1: felt, sv2: felt, t1: felt, t2: felt, t3: felt
) {
    let aIndex = BasicParticle(t1, sv1, fr1);
    let bIndex = BasicParticle(t2, sv2, fr2);
    let (newAIndex, newBIndex) = SemanticMoney.shiftFlow2(aIndex, bIndex, fr3, t3);
    assert newAIndex._flow_rate = fr1 - fr3;
    assert newBIndex._flow_rate = fr2 + fr3;
    return ();
}

@external
func setup_flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            ou_1 = strategy.integers(1, 100), # owned unit for first pd member,
            ou_2 = strategy.integers(1, 100), # owned unit for second pd member,
            fr = strategy.felts(), # default flow rate
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
            t5 = strategy.integers(401, 500), # time 5
            t6 = strategy.integers(501, 600), # time 6
            t7 = strategy.integers(601, 700), # time 7
        )
    %}
    return ();
}

@external
func test_flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    ou_1: felt,
    ou_2: felt,
    fr: felt,
    t1: felt,
    t2: felt,
    t3: felt,
    t4: felt,
    t5: felt,
    t6: felt,
    t7: felt,
) {
    alloc_locals;
    %{ assume(ids.fr * (ids.ou_1 + ids.ou_2) <= 10000000000000000000000000000000000000000) %}

    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(0, BasicParticle(0, 0, 0));
    let pool_member_1 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);

    let (p_index, pool_member_1, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, ou_1, t1
    );

    let (_u_index, _p_index, actualFlowRate) = SemanticMoney.flow2_pd(u_index, p_index, fr, t2);
    assert _u_index._flow_rate = -actualFlowRate;
    assert _p_index._wrapped_particle._flow_rate = actualFlowRate / _p_index.total_units;  // Per Unit
    assert _u_index._settled_at = t2;
    assert _p_index._wrapped_particle._settled_at = t2;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t4
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index._wrapped_particle, t4
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1._synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1._settled_value;

    let pool_member_2 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_2);
    let (p_index, pool_member_2, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, _u_index, ou_2, t6
    );

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_2);
    let (realtime_balance_of_pool_member_mu_2) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index._wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2._synced_particle, t6
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2._settled_value;

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index._wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1._synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1._settled_value;

    let (realtime_balance_of_u_index) = SemanticMoney.realtime_balance_of(u_index, t7);
    assert realtime_balance_of_u_index + realtime_balance_of_pool_member_mu_1 +
        realtime_balance_of_pool_member_mu_2 = 0;
    return ();
}

@external
func setup_shifFlow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            tu = strategy.integers(1, 1000), # time
            fr = strategy.felts(), # flow rate
            t = strategy.integers(0, 100), # time 
        )
    %}
    return ();
}

@external
func test_shifFlow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    tu: felt, fr: felt, t: felt
) {
    alloc_locals;
    %{ assume(ids.fr * ids.tu <= 10000000000000000000000000000000000000000) %}
    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(tu, BasicParticle(0, 0, 0));
    let (_u_index, _p_index, actualFlowRate) = SemanticMoney.shiftFlow_pd(u_index, p_index, fr, t);
    assert _u_index._flow_rate = 0 - actualFlowRate;
    assert _p_index._wrapped_particle._flow_rate = 0 + (actualFlowRate / tu);
    return ();
}

@external
func setup_shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            ou_1 = strategy.integers(1, 100), # owned unit for first pd member,
            ou_2 = strategy.integers(1, 100), # owned unit for second pd member,
            fr = strategy.felts(), # default flow rate
            amount = strategy.felts(), # flow rate of flow,
            t1 = strategy.integers(1, 100), # time 1
            t2 = strategy.integers(101, 200), # time 2
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
            t5 = strategy.integers(401, 500), # time 5
            t6 = strategy.integers(501, 600), # time 6
            t7 = strategy.integers(601, 700), # time 7
        )
    %}
    return ();
}

@external
func test_shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    ou_1: felt,
    ou_2: felt,
    fr: felt,
    amount: felt,
    t1: felt,
    t2: felt,
    t3: felt,
    t4: felt,
    t5: felt,
    t6: felt,
    t7: felt,
) {
    alloc_locals;
    %{ assume(ids.fr <= 10000000000000000000000000000000000) %}
    %{ assume(ids.amount * (ids.ou_1 + ids.ou_2) <= 10000000000000000000000000000000000000000000) %}

    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(0, BasicParticle(0, 0, 0));
    let pool_member_1 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);

    let (p_index, pool_member_1, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, ou_1, t1
    );

    let (_u_index, _p_index, actualAmount) = SemanticMoney.shift2_pd(u_index, p_index, amount);
    assert _u_index._settled_value = (((t2 - t1) * u_index._flow_rate) + u_index._settled_value) -
        actualAmount;
    assert _p_index._wrapped_particle._settled_value = (
        ((t2 - t1) * p_index._wrapped_particle._flow_rate) +
        p_index._wrapped_particle._settled_value
    ) + (actualAmount / p_index.total_units);  // Per Unit

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t3
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index._wrapped_particle, t3
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1._synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1._settled_value;

    let pool_member_2 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_2);
    let (p_index, pool_member_2, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, _u_index, ou_2, t4
    );

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_2);
    let (realtime_balance_of_pool_member_mu_2) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t5
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index._wrapped_particle, t5
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2._synced_particle, t4
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2._settled_value;

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t5
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index._wrapped_particle, t5
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1._synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1._settled_value;

    let (_u_index, _p_index, actualAmount) = SemanticMoney.shift2_pd(u_index, p_index, amount);
    assert _u_index._settled_value = (((t6 - t4) * u_index._flow_rate) + u_index._settled_value) -
        actualAmount;
    assert _p_index._wrapped_particle._settled_value = (
        ((t6 - t4) * p_index._wrapped_particle._flow_rate) +
        p_index._wrapped_particle._settled_value
    ) + (actualAmount / p_index.total_units);  // Per Unit

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_2);
    let (realtime_balance_of_pool_member_mu_2) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index._wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2._synced_particle, t4
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2._settled_value;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index._wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1._synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1._settled_value;

    let (realtime_balance_of_u_index) = SemanticMoney.realtime_balance_of(_u_index, t7);
    assert realtime_balance_of_u_index + realtime_balance_of_pool_member_mu_1 +
        realtime_balance_of_pool_member_mu_2 = 0;
    return ();
}
