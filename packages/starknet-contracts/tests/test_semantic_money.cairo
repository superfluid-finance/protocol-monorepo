%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

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
func test_realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(10, 10, 10);

    let (balance) = SemanticMoney.realtime_balance_of(index, 10);
    assert balance = 10;

    let (_balance) = SemanticMoney.realtime_balance_of(index, 15);
    assert _balance = 60;

    let (_balance) = SemanticMoney.realtime_balance_of(index, 20);
    assert _balance = 110;

    return ();
}

@external
func test_realtime_balance_of_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    let pdPoolIndex = PDPoolIndex(10, BasicParticle(10, 10, 10));
    let pdPoolMember = PDPoolMember(10, 0, BasicParticle(10, 10, 10));
    let pdPoolMemberMU = PDPoolMemberMU(pdPoolIndex, pdPoolMember);
    let (balance) = SemanticMoney.realtime_balance_of_pool_member_mu(pdPoolMemberMU, 20);
    assert balance = 1000;

    let _pdPoolIndex = PDPoolIndex(10, BasicParticle(10, 10, 10));
    let _pdPoolMember = PDPoolMember(5, 100, BasicParticle(10, 10, 10));
    let _pdPoolMemberMU = PDPoolMemberMU(_pdPoolIndex, _pdPoolMember);
    let (_balance) = SemanticMoney.realtime_balance_of_pool_member_mu(_pdPoolMemberMU, 20);
    assert _balance = 600;

    let _pdPoolIndex = PDPoolIndex(10, BasicParticle(10, 10, 10));
    let _pdPoolMember = PDPoolMember(9, 200, BasicParticle(10, 10, 10));
    let _pdPoolMemberMU = PDPoolMemberMU(_pdPoolIndex, _pdPoolMember);
    let (_balance) = SemanticMoney.realtime_balance_of_pool_member_mu(_pdPoolMemberMU, 30);
    assert _balance = 2000;
    return ();
}

@external
func test_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(10, 10, 10);
    let (newIndex) = SemanticMoney.settle(index, 20);
    assert newIndex.rtb_settled_value = 110;
    assert newIndex.rtb_settled_at = 20;

    let (_newIndex) = SemanticMoney.settle(newIndex, 40);
    assert _newIndex.rtb_settled_value = 310;
    assert _newIndex.rtb_settled_at = 40;
    return ();
}

@external
func test_settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let poolIndex = PDPoolIndex(10, BasicParticle(10, 10, 10));
    let (settled_pool_index) = SemanticMoney.settle_for_pool_index(poolIndex, 20);
    assert settled_pool_index.wrapped_particle.rtb_settled_at = 20;
    assert settled_pool_index.wrapped_particle.rtb_settled_value = 110;

    let (_settled_pool_index) = SemanticMoney.settle_for_pool_index(settled_pool_index, 40);
    assert _settled_pool_index.wrapped_particle.rtb_settled_at = 40;
    assert _settled_pool_index.wrapped_particle.rtb_settled_value = 310;
    return ();
}

@external
func test_settle_for_pool_member_mu{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    let poolIndex = PDPoolIndex(10, BasicParticle(10, 10, 10));
    let poolMember = PDPoolMember(5, 10, BasicParticle(10, 10, 10));
    let poolMemberMU = PDPoolMemberMU(poolIndex, poolMember);
    let (settled_poolMemberMU) = SemanticMoney.settle_for_pool_member_mu(poolMemberMU, 20);
    assert settled_poolMemberMU.pdPoolMember.settled_value = 0;
    return ();
}

@external
func test_shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(0, 0, 0);
    let (newIndex, _) = SemanticMoney.shift1(10, index);
    assert newIndex.rtb_settled_value = 10;
    let (_newIndex, _) = SemanticMoney.shift1(10, newIndex);
    assert _newIndex.rtb_settled_value = 20;
    let (__newIndex, _) = SemanticMoney.shift1(-10, _newIndex);
    assert __newIndex.rtb_settled_value = 10;
    let (___newIndex, _) = SemanticMoney.shift1(-10, __newIndex);
    assert ___newIndex.rtb_settled_value = 0;
    return ();
}

@external
func test_shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let aIndex = BasicParticle(0, 0, 0);
    let bIndex = BasicParticle(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.shift2(aIndex, bIndex, 10, 5);
    assert _aIndex.rtb_settled_value + _bIndex.rtb_settled_value = 0;
    let (__aIndex, __bIndex) = SemanticMoney.shift2(_aIndex, _bIndex, 10, 10);
    assert __aIndex.rtb_settled_value + __bIndex.rtb_settled_value = 0;
    return ();
}

@external
func test_setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(0, 0, 0);
    let (newIndex, _) = SemanticMoney.setFlow1(10, index);
    assert newIndex.rtb_flow_rate = 10;
    let (newIndex, _) = SemanticMoney.setFlow1(-10, index);
    assert newIndex.rtb_flow_rate = -10;
    return ();
}

@external
func test_flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let aIndex = BasicParticle(0, 0, 0);
    let bIndex = BasicParticle(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.flow2(aIndex, bIndex, 10, 20);
    assert _aIndex.rtb_flow_rate + _bIndex.rtb_flow_rate = 0;
    let (__aIndex, __bIndex) = SemanticMoney.flow2(_aIndex, _bIndex, 10, 40);
    assert __aIndex.rtb_flow_rate + __bIndex.rtb_flow_rate = 0;
    assert __aIndex.rtb_settled_value = -200;
    assert __bIndex.rtb_settled_value = 200;
    return ();
}

@external
func test_flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    alloc_locals;
    let u_index = BasicParticle(0, 0, 0);
    let p_index = PDPoolIndex(0, BasicParticle(0, 0, 0));
    let pool_member_1 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);

    let unit = 5;
    let time = 10;
    let (p_index, pool_member_1, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, unit, time
    );
    assert p_index.total_units = unit;
    assert p_index.wrapped_particle.rtb_settled_at = time;
    assert pool_member_1.owned_unit = 5;
    assert pool_member_1.synced_particle.rtb_settled_at = time;
    assert pool_member_1.settled_value = 0;

    let value = 100;
    let time = 20;
    let (u_index, p_index) = SemanticMoney.flow2_pd(u_index, p_index, value, time);
    assert u_index.rtb_flow_rate = -value;
    assert u_index.rtb_settled_at = time;
    assert p_index.wrapped_particle.rtb_flow_rate = 20; // Per Unit
    assert p_index.wrapped_particle.rtb_settled_at = time;
    
    let time = 40;
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(pool_member_mu, time);
    assert realtime_balance_of_pool_member_mu_1 = 2000;

    let unit = 5;
    let time = 60;
    let pool_member_2 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_2);
    let (p_index, pool_member_2, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, unit, time
    );
    assert p_index.total_units = 10;
    assert p_index.wrapped_particle.rtb_settled_at = time;
    assert pool_member_2.owned_unit = unit;
    assert pool_member_2.synced_particle.rtb_settled_at = time;
    assert pool_member_2.settled_value = 0;

    let time = 80;
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_2);
    let (realtime_balance_of_pool_member_mu_2) = SemanticMoney.realtime_balance_of_pool_member_mu(pool_member_mu, time);
    assert realtime_balance_of_pool_member_mu_2 = 1000;
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_1);
    let (realtime_balance_of_pool_member_mu_1) = SemanticMoney.realtime_balance_of_pool_member_mu(pool_member_mu, time);
    assert realtime_balance_of_pool_member_mu_1 = 5000;

    let (realtime_balance_of_u_index) = SemanticMoney.realtime_balance_of(u_index, time);
    assert realtime_balance_of_u_index + realtime_balance_of_pool_member_mu_1 + realtime_balance_of_pool_member_mu_2 = 0;

    // Test for Alignment
    let unit = 3;
    let time = 100;
    let pool_member_3 = PDPoolMember(0, 0, BasicParticle(0, 0, 0));
    let pool_member_mu = PDPoolMemberMU(p_index, pool_member_3);
    let (p_index, pool_member_3, u_index) = SemanticMoney.pool_member_update(
        pool_member_mu, u_index, unit, time
    );
    assert p_index.total_units = 13;
    assert p_index.wrapped_particle.rtb_settled_at = time;
    assert pool_member_3.owned_unit = unit;
    assert pool_member_3.synced_particle.rtb_settled_at = time;
    assert pool_member_3.settled_value = 0;
    assert u_index.rtb_flow_rate = -91;
    return ();
}
