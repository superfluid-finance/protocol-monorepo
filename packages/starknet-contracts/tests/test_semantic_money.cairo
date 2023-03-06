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
    // If `settled_at` of b of greater
    let a = BasicParticle(5, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle.rtb_flow_rate = 20;
    assert newBasicParticle.rtb_settled_at = 10;
    assert newBasicParticle.rtb_settled_value = 70;

    // if `settled_at` of a and b are same
    let a = BasicParticle(10, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle.rtb_flow_rate = 20;
    assert newBasicParticle.rtb_settled_at = 10;
    assert newBasicParticle.rtb_settled_value = 20;

    // if `settled_at` of a of greater
    let a = BasicParticle(20, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle.rtb_flow_rate = 20;
    assert newBasicParticle.rtb_settled_at = 20;
    assert newBasicParticle.rtb_settled_value = 120;

    let (_newBasicParticle) = SemanticMoney.mappend(b, newBasicParticle);
    assert _newBasicParticle.rtb_flow_rate = 30;
    assert _newBasicParticle.rtb_settled_at = 20;
    assert _newBasicParticle.rtb_settled_value = 230;
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
            t3 = strategy.integers(201, 300), # time 3
            t4 = strategy.integers(301, 400), # time 4
        )
    %}
    return ();
}

@external
func test_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, t1: felt, t2: felt, t3: felt, t4: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex) = SemanticMoney.settle(index, t2);
    assert newIndex.rtb_settled_value = ((t2 - t1) * fr) + sv;
    assert newIndex.rtb_settled_at = t2;

    let (_newIndex) = SemanticMoney.settle(newIndex, t3);
    assert _newIndex.rtb_settled_value = ((t3 - t2) * fr) + ((t2 - t1) * fr) + sv;
    assert _newIndex.rtb_settled_at = t3;

    let (__newIndex) = SemanticMoney.settle(newIndex, t4);
    assert __newIndex.rtb_settled_value = ((t4 - t3) * fr) + (
        ((t3 - t2) * fr) + ((t2 - t1) * fr) + sv
    );
    assert __newIndex.rtb_settled_at = t4;
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
    assert settled_pool_index.wrapped_particle.rtb_settled_at = t2;
    assert settled_pool_index.wrapped_particle.rtb_settled_value = ((t2 - t1) * fr) + sv;

    let (_settled_pool_index) = SemanticMoney.settle_for_pool_index(settled_pool_index, t3);
    assert _settled_pool_index.wrapped_particle.rtb_settled_at = t3;
    assert _settled_pool_index.wrapped_particle.rtb_settled_value = ((t3 - t2) * fr) + (
        ((t2 - t1) * fr) + sv
    );

    let (__settled_pool_index) = SemanticMoney.settle_for_pool_index(_settled_pool_index, t4);
    assert __settled_pool_index.wrapped_particle.rtb_settled_at = t4;
    assert __settled_pool_index.wrapped_particle.rtb_settled_value = ((t4 - t3) * fr) + (
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
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t1, sv_wrapped, fr), t2
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        BasicParticle(t1, sv_synced, fr), t2
    );
    assert settled_poolMemberMU.pdPoolMember.settled_value = (
        balance_for_wrapped_particle - balance_for_synced_particle
    ) * ou;
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
    assert newIndex.rtb_settled_value = amount + sv;
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
    let (_aIndex, _bIndex) = SemanticMoney.shift2(aIndex, bIndex, amount, t1);
    assert _aIndex.rtb_settled_value = (((t1 - _aIndex.rtb_settled_at) * _aIndex.rtb_settled_at) + 0) - amount;
    assert _bIndex.rtb_settled_value = (((t1 - _bIndex.rtb_settled_at) * _bIndex.rtb_settled_at) + 0) + amount;
    assert _aIndex.rtb_settled_value + _bIndex.rtb_settled_value = 0;
    return ();
}

@external
func setup_setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
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
func test_setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, fr_1: felt, t1: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex, _) = SemanticMoney.setFlow1(fr_1, index);
    assert newIndex.rtb_flow_rate = fr_1;
    return ();
}

@external
func setup_shiftFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
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
func test_shiftFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, sv: felt, fr_1: felt, t1: felt
) {
    let index = BasicParticle(t1, sv, fr);
    let (newIndex, _) = SemanticMoney.shiftFlow1(fr_1, index);
    assert newIndex.rtb_flow_rate = fr_1 + fr;
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
    assert _aIndex.rtb_flow_rate = -fr_1;
    assert _bIndex.rtb_flow_rate = fr_1;

    let (balance_for_sender_particle) = SemanticMoney.realtime_balance_of(_aIndex, t2);
    let (balance_for_receiver_particle) = SemanticMoney.realtime_balance_of(_bIndex, t2);
    assert balance_for_sender_particle + balance_for_receiver_particle = 0;
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
    %{ assume(ids.fr < 680564733841876926926749214863536422912) %}
    %{ assume(ids.fr * ids.ou_1 <= 35826760283823081323735869139555149560624823914174224752208832238969029904) %}
    %{ assume(ids.fr * ids.ou_2 <= 35826760283823081323735869139555149560624823914174224752208832238969029904) %}

    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(0, BasicParticle(0, 0, 0));
    let pool_member_1 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);

    let (p_index, pool_member_1, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, ou_1, t1
    );

    let (_u_index, _p_index, _) = SemanticMoney.flow2_pd(u_index, p_index, fr, t2);
    assert _u_index.rtb_flow_rate = u_index.rtb_flow_rate - ((fr / p_index.total_units) * p_index.total_units);
    assert _p_index.wrapped_particle.rtb_flow_rate = p_index.wrapped_particle.rtb_flow_rate + (
        ((fr / p_index.total_units) * p_index.total_units) / p_index.total_units
    );  // Per Unit
    assert _u_index.rtb_settled_at = t2;
    assert _p_index.wrapped_particle.rtb_settled_at = t2;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t4
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index.wrapped_particle, t4
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1.synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1.settled_value;

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
        p_index.wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2.synced_particle, t6
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2.settled_value;

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index.wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1.synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1.settled_value;

    let (realtime_balance_of_u_index) = SemanticMoney.realtime_balance_of(
        u_index, t7
    );
    assert realtime_balance_of_u_index + realtime_balance_of_pool_member_mu_1 + realtime_balance_of_pool_member_mu_2 = 0;
    return ();
}

@external
func setup_shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            ou_1 = strategy.integers(1, 100), # owned unit for first pd member,
            ou_2 = strategy.integers(1, 100), # owned unit for second pd member,
            ou_3 = strategy.integers(1, 100), # owned unit for third pd member,
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
    ou_3: felt,
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
    %{ assume(ids.fr < 680564733841876926926749214863536422912) %}
    %{ assume(ids.fr * ids.ou_1 <= 35826760283823081323735869139555149560624823914174224752208832238969029904) %}
    %{ assume(ids.fr * ids.ou_2 <= 35826760283823081323735869139555149560624823914174224752208832238969029904) %}

    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(0, BasicParticle(0, 0, 0));
    let pool_member_1 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);

    let (p_index, pool_member_1, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, ou_1, t1
    );

    let (_u_index, _p_index, _) = SemanticMoney.shift2_pd(u_index, p_index, amount, t2);
    assert _u_index.rtb_settled_value = (
        ((t2 - t1) * u_index.rtb_flow_rate) + u_index.rtb_settled_value
    ) - ((amount / p_index.total_units) * p_index.total_units);
    assert _u_index.rtb_settled_at = t2;
    assert _p_index.wrapped_particle.rtb_settled_value = (
        ((t2 - t1) * p_index.wrapped_particle.rtb_flow_rate) +
        p_index.wrapped_particle.rtb_settled_value
    ) + (((amount / p_index.total_units) * p_index.total_units) / p_index.total_units);  // Per Unit
    assert _p_index.wrapped_particle.rtb_settled_at = t2;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t3
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index.wrapped_particle, t3
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1.synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1.settled_value;

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
        p_index.wrapped_particle, t5
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2.synced_particle, t4
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2.settled_value;

    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t5
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        p_index.wrapped_particle, t5
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1.synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1.settled_value;

    let (_u_index, _p_index, _) = SemanticMoney.shift2_pd(u_index, p_index, amount, t6);
    assert _u_index.rtb_settled_value = (
        ((t6 - t4) * u_index.rtb_flow_rate) + u_index.rtb_settled_value
    ) - ((amount / p_index.total_units) * p_index.total_units);
    assert _u_index.rtb_settled_at = t6;
    assert _p_index.wrapped_particle.rtb_settled_value = (
        ((t6 - t4) * p_index.wrapped_particle.rtb_flow_rate) +
        p_index.wrapped_particle.rtb_settled_value
    ) + (((amount / p_index.total_units) * p_index.total_units) / p_index.total_units);  // Per Unit
    assert _p_index.wrapped_particle.rtb_settled_at = t6;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_2);
    let (realtime_balance_of_pool_member_mu_2) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index.wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_2.synced_particle, t4
    );
    assert realtime_balance_of_pool_member_mu_2 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_2
    ) + pool_member_2.settled_value;

    let pool_member_mu = PDPoolMemberMU(_p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(
        pool_member_mu, t7
    );
    let (balance_for_wrapped_particle) = SemanticMoney.realtime_balance_of(
        _p_index.wrapped_particle, t7
    );
    let (balance_for_synced_particle) = SemanticMoney.realtime_balance_of(
        pool_member_1.synced_particle, t2
    );
    assert realtime_balance_of_pool_member_mu_1 = (
        (balance_for_wrapped_particle - balance_for_synced_particle) * ou_1
    ) + pool_member_1.settled_value;

    let (realtime_balance_of_u_index) = SemanticMoney.realtime_balance_of(
        _u_index, t7
    );
    assert realtime_balance_of_u_index + realtime_balance_of_pool_member_mu_1 + realtime_balance_of_pool_member_mu_2 = 0;
    return ();
}
