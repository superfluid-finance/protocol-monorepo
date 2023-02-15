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
    let a = BasicParticle(5, 10, 10);
    let b = BasicParticle(10, 10, 10);
    let (newBasicParticle) = SemanticMoney.mappend(a, b);
    assert newBasicParticle.rtb_flow_rate = 20;
    assert newBasicParticle.rtb_settled_at = 10;
    assert newBasicParticle.rtb_settled_value = 70;
    let (_newBasicParticle) = SemanticMoney.mappend(b, newBasicParticle);
    assert _newBasicParticle.rtb_flow_rate = 30;
    assert _newBasicParticle.rtb_settled_at = 10;
    assert _newBasicParticle.rtb_settled_value = 80;
    let _a = BasicParticle(50, 10, 10);
    let _b = BasicParticle(10, 10, 10);
    let (__newBasicParticle) = SemanticMoney.mappend(_a, _b);
    assert __newBasicParticle.rtb_flow_rate = 20;
    assert __newBasicParticle.rtb_settled_at = 50;
    assert __newBasicParticle.rtb_settled_value = 420;
    return ();
}

@external
func test_realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(0, 0, 10);
    let (balance) = SemanticMoney.realtime_balance_of(index, 10);
    assert balance = 100;
    let (_balance) = SemanticMoney.realtime_balance_of(index, 15);
    assert _balance = 150;
    return ();
}

@external
func test_realtime_balance_of_pool_member{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    let pdPoolIndex = PDPoolIndex(10, BasicParticle(0, 0, 1));
    let pdPoolMember = PDPoolMember(10, 0, BasicParticle(0, 0, 1));
    let pdPoolMemberMU = PDPoolMemberMU(pdPoolIndex, pdPoolMember);
    let (balance) = SemanticMoney.realtime_balance_of_pool_member(pdPoolMemberMU, 10);
    assert balance = 100;
    let _pdPoolIndex = PDPoolIndex(10, BasicParticle(0, 0, 10));
    let _pdPoolMember = PDPoolMember(10, 0, BasicParticle(0, 0, 10));
    let _pdPoolMemberMU = PDPoolMemberMU(_pdPoolIndex, _pdPoolMember);
    let (_balance) = SemanticMoney.realtime_balance_of_pool_member(_pdPoolMemberMU, 10);
    assert _balance = 1000;
    let __pdPoolIndex = PDPoolIndex(10, BasicParticle(0, 0, 10));
    let __pdPoolMember = PDPoolMember(3, 0, BasicParticle(0, 0, 10));
    let __pdPoolMemberMU = PDPoolMemberMU(__pdPoolIndex, __pdPoolMember);
    let (__balance) = SemanticMoney.realtime_balance_of_pool_member(__pdPoolMemberMU, 10);
    assert __balance = 300;
    return ();
}

@external
func test_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = BasicParticle(0, 0, 10);
    let (newIndex) = SemanticMoney.settle(index, 10);
    assert newIndex.rtb_settled_value = 100;
    assert newIndex.rtb_settled_at = 10;
    let (_newIndex) = SemanticMoney.settle(newIndex, 20);
    assert _newIndex.rtb_settled_value = 200;
    assert _newIndex.rtb_settled_at = 20;
    return ();
}

@external
func test_settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let poolIndex = PDPoolIndex(10, BasicParticle(0, 0, 10));
    let (settled_pool_index) = SemanticMoney.settle_for_pool_index(poolIndex, 10);
    assert settled_pool_index.wrapped_particle.rtb_settled_at = 10;
    assert settled_pool_index.wrapped_particle.rtb_settled_value = 100;
    let (_settled_pool_index) = SemanticMoney.settle_for_pool_index(settled_pool_index, 20);
    assert _settled_pool_index.wrapped_particle.rtb_settled_at = 20;
    assert _settled_pool_index.wrapped_particle.rtb_settled_value = 200;
    return ();
}

@external
func test_settle_for_pool_member_mu{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let poolIndex = PDPoolIndex(10, BasicParticle(0, 0, 10));
    let poolMember = PDPoolMember(5, 0, BasicParticle(0, 0, 10));
    let poolMemberMU = PDPoolMemberMU(poolIndex, poolMember);
    let (settled_poolMemberMU) = SemanticMoney.settle_for_pool_member_mu(poolMemberMU, 500);
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
