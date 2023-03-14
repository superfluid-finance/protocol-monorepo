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
from src.interfaces.ISuperTokenPool import ISuperTokenPool

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
        context.supertoken_contract_address = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", [1539470638642759296633, 21332, 18, 524240933577613431689919163345126223950080805481049702978908196600880894530]).contract_address
        context.MINT_AMOUNT = 1000000000000000000
    %}
    return ();
}

@external
func setup_flow1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr1 = strategy.integers(1, 1000000),
            fr2 = strategy.integers(1, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_flow1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}

    ISuperToken.mint(
        contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT
    );
    let (balance) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    assert balance = MINT_AMOUNT;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account2,
        flowId=0,
        flowRate=fr1,
    );
    ISuperToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr2,
    );
    %{ stop_prank_callable() %}

    let (timestamp) = get_block_timestamp();
    let extraTime = 20;
    %{ stop_warp = warp(ids.timestamp + ids.extraTime, ids.supertoken_contract_address) %}
    let (balanceOfSender) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfReceiver1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfReceiver2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    %{ stop_warp() %}
    let (balance) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    assert balanceOfSender = balance - ((fr1 + fr2) * extraTime);
    assert balanceOfReceiver1 = (fr1 * extraTime);
    assert balanceOfReceiver2 = (fr2 * extraTime);
    return ();
}

@external
func setup_flow2to1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr1 = strategy.integers(1, 1000000),
            fr2 = strategy.integers(1, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_flow2to1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}

    ISuperToken.mint(
        contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT
    );
    ISuperToken.mint(
        contract_address=supertoken_contract_address, receiver=account2, amount=MINT_AMOUNT
    );
    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    assert balanceOfAccount1 = MINT_AMOUNT;
    assert balanceOfAccount2 = MINT_AMOUNT;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr1,
    );
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account2,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr2,
    );
    %{ stop_prank_callable() %}

    let (timestamp) = get_block_timestamp();
    let extraTime = 20;
    %{ stop_warp = warp(ids.timestamp + ids.extraTime, ids.supertoken_contract_address) %}
    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    %{ stop_warp() %}
    assert balanceOfAccount1 = MINT_AMOUNT - (fr1 * extraTime);
    assert balanceOfAccount2 = MINT_AMOUNT - (fr2 * extraTime);
    assert balanceOfAccount3 = ((fr2 + fr1) * extraTime);
    return ();
}

@external
func setup_distribute1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            amount = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_distribute1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt, u1: felt, u2: felt, account1: felt, account2: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    let (contract_address) = get_contract_address();
    ISuperToken.mint(
        contract_address=supertoken_contract_address, receiver=contract_address, amount=MINT_AMOUNT
    );
    let (balance) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=contract_address
    );
    assert balance = MINT_AMOUNT;

    let (contract_address) = get_contract_address();
    let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account1, unit=u1);
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account2, unit=u2);

    ISuperToken.distribute(contract_address=supertoken_contract_address, senderAddress=contract_address, poolAddress=pool, reqAmount=amount);

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (balanceOfContract) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=contract_address
    );
    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    return ();
}
