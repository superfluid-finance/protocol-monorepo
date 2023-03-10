%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import get_contract_address, get_block_timestamp

from src.tokens.ERC20x.SuperToken.library import SuperToken
from src.utils.SemanticMoney import (
    SemanticMoney,
    BasicParticle,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
)
from src.interfaces.ISuperToken import ISuperToken
from src.interfaces.IPool import IPool

from protostar.asserts import (
    assert_eq,
    assert_not_eq,
    assert_signed_lt,
    assert_signed_le,
    assert_signed_gt,
    assert_unsigned_lt,
    assert_unsigned_le,
    assert_unsigned_gt,
    assert_signed_ge,
    assert_unsigned_ge,
)

@external
func __setup__{syscall_ptr: felt*}() {
    let (contract_address) = get_contract_address();
    %{
        declare("./src/pools/PoolImpl.cairo")
        context.supertoken_contract_address = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", [1539470638642759296633, 21332, 18, 2235168687633376128624331135262374777040113007795923927240436837933471963260]).contract_address
    %}
    return ();
}

@external
func setup_token{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.integers(1, 1000000000000000000000000),
            flow_id = strategy.integers(1, 1000000000000000000000000),
            unit1 = strategy.integers(1, 1000000000000000000000000),
            unit2 = strategy.integers(1, 1000000000000000000000000),
            amount1 = strategy.integers(1, 1000000000000000000000000),
            amount2 = strategy.integers(1, 1000000000000000000000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000)
        )
    %}
    return ();
}

// @external
// func test_token{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     fr: felt,
//     flow_id: felt,
//     unit1: felt,
//     unit2: felt,
//     amount1: felt,
//     amount2: felt,
//     account1: felt,
//     account2: felt,
//     account3: felt,
// ) {
//     %{ assume(ids.account2 != ids.account3) %}

// let (contract_address) = get_contract_address();

// tempvar supertoken_contract_address;
//     %{ ids.supertoken_contract_address = context.supertoken_contract_address %}
//     let (balance) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     assert balance = 0;
//     ISuperToken.mint(
//         contract_address=supertoken_contract_address, receiver=contract_address, amount=amount1
//     );
//     let (balance) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     let (balanceOfZeroAddress) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=0
//     );
//     assert balance = amount1;
//     assert balanceOfZeroAddress = -amount1;

// // Universal Index Operations
//     let (success) = ISuperToken.flow(
//         contract_address=supertoken_contract_address,
//         senderAddress=contract_address,
//         receiverAddress=account1,
//         flowId=flow_id,
//         flowRate=fr,
//     );
//     let (balanceBefore) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     let (balanceOfReceiverBefore) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     let (timestamp) = get_block_timestamp();
//     %{ stop_warp = warp(ids.timestamp + 20, ids.supertoken_contract_address) %}
//     let (balanceAfter) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     let (balanceOfReceiverAfter) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     %{ stop_warp() %}
//     assert balanceAfter = balanceBefore - (fr * 20);
//     assert balanceOfReceiverAfter = balanceOfReceiverBefore + (fr * 20);

// // Proportional Distribution Pool Operations(Distribute)
//     let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
//     let (pool_index) = IPool.getIndex(contract_address=pool);
//     assert pool_index.total_units = 0;
//     assert pool_index.wrapped_particle.flow_rate = 0;
//     assert pool_index.wrapped_particle.rtb_settled_value = 0;
//     %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
//     ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
//     %{ stop_prank_callable() %}
//     IPool.updatePoolMember(contract_address=pool, memberAddress=account2, unit=unit1);
//     let (member1) = IPool.getMember(contract_address=pool, memberAddress=account2);
//     assert member1.owned_unit = unit1;
//     let (pool_index) = IPool.getIndex(contract_address=pool);
//     assert pool_index.total_units = unit1;
//     let (_, actualAmount) = ISuperToken.distribute(
//         contract_address=supertoken_contract_address,
//         senderAddress=contract_address,
//         poolAddress=pool,
//         reqAmount=amount2,
//     );
//     let (balanceOfDistributor) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     let (balanceOfMemberOfPool) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     assert balanceOfDistributor = amount1 - actualAmount;
//     assert balanceOfMemberOfPool = member1.owned_unit * (actualAmount / pool_index.total_units);

// // Proportional Distribution Pool Operations(Distribute Flow)
//     %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
//     ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
//     %{ stop_prank_callable() %}
//     IPool.updatePoolMember(contract_address=pool, memberAddress=account3, unit=unit2);
//     let (member2) = IPool.getMember(contract_address=pool, memberAddress=account3);
//     let (pool_index) = IPool.getIndex(contract_address=pool);
//     assert pool_index.total_units = unit1 + unit2;
//     assert pool_index.wrapped_particle.flow_rate = 0;
//     let (_, actualFlowRate) = ISuperToken.distributeFlow(
//         contract_address=supertoken_contract_address,
//         senderAddress=contract_address,
//         poolAddress=pool,
//         flowId=flow_id,
//         reqFlowRate=fr,
//     );
//     let (pool_index) = IPool.getIndex(contract_address=pool);
//     assert pool_index.wrapped_particle.flow_rate = (fr/pool_index.total_units) * pool_index.total_units;
//     let (member2) = IPool.getMember(contract_address=pool, memberAddress=account3);
//     assert member2.synced_particle.flow_rate = (fr/pool_index.total_units) * pool_index.total_units;
//     %{ stop_warp = warp(ids.timestamp + 20, ids.supertoken_contract_address) %}
//     let (balanceOfDistributor) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=contract_address
//     );
//     let (balanceOfMemberOfPool) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (balanceOfMemberOfPool2) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account3
//     );
//     %{ stop_warp() %}
//     assert balanceOfDistributor = (amount1 - actualAmount - ((fr + actualFlowRate) * 20));
//     let (initialBalanceOfMemberOfPool) = ISuperToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (member1) = IPool.getMember(contract_address=pool, memberAddress=account2);
//     let (member2) = IPool.getMember(contract_address=pool, memberAddress=account3);
//     // assert_unsigned_lt(initialBalanceOfMemberOfPool, balanceOfMemberOfPool);
//     // assert balanceOfMemberOfPool2 = member2.owned_unit * member2.synced_particle.flow_rate * 20;
//     return ();
// }
