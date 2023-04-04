%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import get_contract_address, get_block_timestamp
from starkware.cairo.common.math import unsigned_div_rem
from starkware.cairo.common.bool import TRUE, FALSE

from src.tokens.ERC20x.SuperToken.library import SuperToken
from src.utils.SemanticMoney import (
    SemanticMoney,
    BasicParticle,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
)
from src.interfaces.ISuperToken import ISuperToken
from src.interfaces.ISuperTokenPool import ISuperTokenPool, ISuperTokenPoolAdmin

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
        context.supertoken_contract_address = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", [1539470638642759296633, 21332, 18, 1967013752834806001269811315755539563695215919214241724661593146835538551452]).contract_address
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
            t1 = strategy.integers(1, 1000)
        )
    %}
    return ();
}

@external
func test_flow1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt, t1: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    let (balance) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = 0;
    assert netFlowRateOfAccount2 = 0;
    assert netFlowRateOfAccount3 = 0;

    let (flowRateOfAccount1andAccount2) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );
    let (flowRateOfAccount1andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = 0;
    assert flowRateOfAccount1andAccount3 = 0;

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

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = -(fr1 + fr2);
    assert netFlowRateOfAccount2 = fr1;
    assert netFlowRateOfAccount3 = fr2;

    let (flowRateOfAccount1andAccount2) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );
    let (flowRateOfAccount1andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = fr1;
    assert flowRateOfAccount1andAccount3 = fr2;

    let (timestamp) = get_block_timestamp();
    let t2 = timestamp + t1;
    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
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
    // The value of balanceOfSender is -((fr1 + fr2) * (t2 - timestamp)) but you can't have a negative balance so 0 is returned
    assert balanceOfSender = 0;
    assert balanceOfReceiver1 = (fr1 * (t2 - timestamp));
    assert balanceOfReceiver2 = (fr2 * (t2 - timestamp));
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
            t1 = strategy.integers(1, 1000)
        )
    %}
    return ();
}

@external
func test_flow2to1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt, t1: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = 0;
    assert netFlowRateOfAccount2 = 0;
    assert netFlowRateOfAccount3 = 0;

    let (flowRateOfAccount1andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );
    let (flowRateOfAccount2andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account2, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount3 = 0;
    assert flowRateOfAccount2andAccount3 = 0;

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

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = -fr1;
    assert netFlowRateOfAccount2 = -fr2;
    assert netFlowRateOfAccount3 = fr1 + fr2;

    let (flowRateOfAccount1andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );
    let (flowRateOfAccount2andAccount3) = ISuperToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account2, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount3 = fr1;
    assert flowRateOfAccount2andAccount3 = fr2;

    let (timestamp) = get_block_timestamp();
    let t2 = t1 + timestamp;
    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
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
    assert balanceOfAccount1 = 0;
    assert balanceOfAccount2 = 0;
    assert balanceOfAccount3 = ((fr2 + fr1) * (t2 - timestamp));
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
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000)
        )
    %}
    return ();
}

@external
func test_distribute1to2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt
) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account2 != ids.account3) %}
    %{ assume(ids.account1 != ids.account3) %}
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

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
    let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = 0;

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
    let (member_data_1) = ISuperTokenPool.getMember(contract_address=pool, memberAddress=account2);
    assert member_data_1.owned_unit = u1;
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account3, unit=u2);
    let (member_data_2) = ISuperTokenPool.getMember(contract_address=pool, memberAddress=account3);
    assert member_data_2.owned_unit = u2;
    %{ stop_prank_callable() %}

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = u1 + u2;

    let tu = u1 + u2;

    let (poolIndex) = ISuperTokenPool.getIndex(contract_address=pool);
    assert poolIndex.total_units = tu;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.distribute(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        reqAmount=amount,
    );
    %{ stop_prank_callable() %}

    let (isConnectedToPool) = ISuperTokenPoolAdmin.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=account2
    );
    let (connectedPools) = ISuperToken.getNumConnections(
        contract_address=supertoken_contract_address, account=account2
    );
    assert isConnectedToPool = FALSE;
    assert connectedPools = 0;

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (isConnectedToPool) = ISuperTokenPoolAdmin.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=account2
    );
    let (connectedPools) = ISuperToken.getNumConnections(
        contract_address=supertoken_contract_address, account=account2
    );
    assert isConnectedToPool = TRUE;
    assert connectedPools = 1;

    let (isConnectedToPool) = ISuperTokenPoolAdmin.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=account3
    );
    let (connectedPools) = ISuperToken.getNumConnections(
        contract_address=supertoken_contract_address, account=account3
    );
    assert isConnectedToPool = FALSE;
    assert connectedPools = 0;

    %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (isConnectedToPool) = ISuperTokenPoolAdmin.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=account3
    );
    let (connectedPools) = ISuperToken.getNumConnections(
        contract_address=supertoken_contract_address, account=account3
    );
    assert isConnectedToPool = TRUE;
    assert connectedPools = 1;

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = 0;

    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    let (quotient, _) = unsigned_div_rem(amount, tu);
    let actualAmount = quotient * tu;
    let amountPerUnit = actualAmount / tu;
    assert balanceOfAccount1 = MINT_AMOUNT - actualAmount;
    assert balanceOfAccount2 = amountPerUnit * u1;
    assert balanceOfAccount3 = amountPerUnit * u2;
    return ();
}

@external
func setup_distribute1to2flow_both_connected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            fr = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            t1 = strategy.integers(1, 10000),
        )
    %}
    return ();
}

@external
func test_distribute1to2flow_both_connected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(fr: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt, t1: felt) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account2 != ids.account3) %}
    %{ assume(ids.account1 != ids.account3) %}
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account3, unit=u2);
    %{ stop_prank_callable() %}

    let (timestamp) = get_block_timestamp();

    let (claimableForAccount2) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=timestamp, memberAddress=account2
    );
    let (claimableForAccount3) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=timestamp, memberAddress=account3
    );
    assert claimableForAccount2 = 0;
    assert claimableForAccount3 = 0;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=fr,
    );
    %{ stop_prank_callable() %}

    let tu = u1 + u2;

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = tu;

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (claimableForAccount2) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=timestamp, memberAddress=account2
    );
    let (claimableForAccount3) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=timestamp, memberAddress=account3
    );
    assert claimableForAccount2 = 0;
    assert claimableForAccount3 = 0;
    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = 0;

    let t2 = t1 + timestamp;

    let (quotient, _) = unsigned_div_rem(fr, tu);
    let actualFlowRate = quotient * tu;
    let flowRatePerUnit = actualFlowRate / tu;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    let (distributionFlowRate) = ISuperTokenPool.getDistributionFlowRate(contract_address=pool);
    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );
    assert distributionFlowRate = actualFlowRate;
    assert netFlowRateOfAccount1 = -actualFlowRate;
    assert netFlowRateOfAccount2 = flowRatePerUnit * u1;
    assert netFlowRateOfAccount3 = flowRatePerUnit * u2;
    assert netFlowRateOfAccount2 + netFlowRateOfAccount3 = distributionFlowRate;
    assert netFlowRateOfAccount1 + netFlowRateOfAccount2 + netFlowRateOfAccount3 = 0;

    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    // The value should be -((t2 - timestamp) * actualFlowRate) but negative values are represented as 0
    assert balanceOfAccount1 = 0;
    assert balanceOfAccount2 = t2 * netFlowRateOfAccount2;
    assert balanceOfAccount3 = t2 * netFlowRateOfAccount3;
    %{ stop_warp() %}
    return ();
}

@external
func setup_distribute1to2flow_one_connected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            fr = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            t1 = strategy.integers(1, 100),
        )
    %}
    return ();
}

@external
func test_distribute1to2flow_one_connected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(fr: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt, t1: felt) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account2 != ids.account3) %}
    %{ assume(ids.account1 != ids.account3) %}
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account3, unit=u2);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=fr,
    );
    %{ stop_prank_callable() %}

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = u1 + u2;

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (pendingUnits) = ISuperTokenPool.getPendingUnits(contract_address=pool);
    assert pendingUnits = u2;

    let (timestamp) = get_block_timestamp();
    let t2 = t1 + timestamp;

    let tu = u1 + u2;
    let (quotient, _) = unsigned_div_rem(fr, tu);
    let actualFlowRate = quotient * tu;
    let flowRatePerUnit = actualFlowRate / tu;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    let (distributionFlowRate) = ISuperTokenPool.getDistributionFlowRate(contract_address=pool);
    let (pendingDistributionFlowRate) = ISuperTokenPool.getPendingDistributionFlowRate(
        contract_address=pool
    );

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );
    assert distributionFlowRate = actualFlowRate;
    assert netFlowRateOfAccount1 = -actualFlowRate;
    assert netFlowRateOfAccount2 = flowRatePerUnit * u1;
    assert netFlowRateOfAccount3 = 0;
    assert netFlowRateOfAccount2 + pendingDistributionFlowRate = distributionFlowRate;
    assert netFlowRateOfAccount1 + netFlowRateOfAccount2 + pendingDistributionFlowRate = 0;

    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    // The value should be -((t2 - timestamp) * actualFlowRate) but negative values are represented as 0
    assert balanceOfAccount1 = 0;
    assert balanceOfAccount2 = (t2 - timestamp) * netFlowRateOfAccount2;
    assert balanceOfAccount3 = 0;
    %{ stop_warp() %}
    return ();
}

@external
func setup_distribute2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            t1 = strategy.integers(1, 100),
        )
    %}
    return ();
}

@external
func test_distribute2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt, t1: felt
) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account2 != ids.account3) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.u1 > 0) %}
    %{ assume(ids.u2 > 0) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (pool1) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    let (pool2) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool1) %}
    ISuperTokenPool.updateMember(contract_address=pool1, memberAddress=account3, unit=u1);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.pool2) %}
    ISuperTokenPool.updateMember(contract_address=pool2, memberAddress=account3, unit=u2);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool1,
        flowId=0,
        reqFlowRate=fr,
    );
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account2,
        poolAddress=pool2,
        flowId=0,
        reqFlowRate=fr,
    );
    %{ stop_prank_callable() %}

    let (pendingUnitsPool1) = ISuperTokenPool.getPendingUnits(contract_address=pool1);
    assert pendingUnitsPool1 = u1;

    let (pendingUnitsPool2) = ISuperTokenPool.getPendingUnits(contract_address=pool2);
    assert pendingUnitsPool2 = u2;

    %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool1);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool2);
    %{ stop_prank_callable() %}

    let (pendingUnitsPool1) = ISuperTokenPool.getPendingUnits(contract_address=pool1);
    assert pendingUnitsPool1 = 0;

    let (pendingUnitsPool2) = ISuperTokenPool.getPendingUnits(contract_address=pool2);
    assert pendingUnitsPool2 = 0;

    let (timestamp) = get_block_timestamp();
    let t2 = t1 + timestamp;

    let (quotientForPool1, _) = unsigned_div_rem(fr, u1);
    let actualFlowRateForPool1 = quotientForPool1 * u1;
    let flowRatePerUnitForPool1 = actualFlowRateForPool1 / u1;

    let (quotientForPool2, _) = unsigned_div_rem(fr, u2);
    let actualFlowRateForPool2 = quotientForPool2 * u2;
    let flowRatePerUnitForPool2 = actualFlowRateForPool2 / u2;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    let (distributionFlowRateForPool1) = ISuperTokenPool.getDistributionFlowRate(
        contract_address=pool1
    );
    let (distributionFlowRateForPool2) = ISuperTokenPool.getDistributionFlowRate(
        contract_address=pool2
    );

    let (pendingDistributionFlowRateForPool1) = ISuperTokenPool.getPendingDistributionFlowRate(
        contract_address=pool1
    );
    let (pendingDistributionFlowRateForPool2) = ISuperTokenPool.getPendingDistributionFlowRate(
        contract_address=pool2
    );

    let (netFlowRateOfAccount1) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert distributionFlowRateForPool1 = actualFlowRateForPool1;
    assert distributionFlowRateForPool2 = actualFlowRateForPool2;

    assert netFlowRateOfAccount1 = -actualFlowRateForPool1;
    assert netFlowRateOfAccount2 = -actualFlowRateForPool2;

    assert netFlowRateOfAccount3 = (
        (flowRatePerUnitForPool1 * u1) + (flowRatePerUnitForPool2 * u2)
    );

    assert netFlowRateOfAccount1 + netFlowRateOfAccount2 + netFlowRateOfAccount3 = 0;

    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    // The value should be -((t2 - timestamp) * actualFlowRate) but negative values are represented as 0
    assert balanceOfAccount1 = 0;
    assert balanceOfAccount2 = 0;
    assert balanceOfAccount3 = (t2 - timestamp) * netFlowRateOfAccount3;
    %{ stop_warp() %}
    return ();
}

@external
func setup_pool_multiple_claims{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            t1 = strategy.integers(1, 1000),
        )
    %}
    return ();
}

@external
func test_pool_multiple_claims{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr: felt, u1: felt, account1: felt, account2: felt, t1: felt
) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.u1 > 0) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (pool) = ISuperToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperTokenPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=fr,
    );
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperToken.connectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}

    let (timestamp) = get_block_timestamp();
    let t2 = t1 + timestamp;

    let (quotient, _) = unsigned_div_rem(fr, u1);
    let actualFlowRate = quotient * u1;
    let flowRatePerUnit = actualFlowRate / u1;

    let (balanceOfAccount1) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    assert balanceOfAccount1 = 0;
    let (balanceOfAccount2) = ISuperToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    assert balanceOfAccount2 = 0;

    %{ stop_warp = warp(ids.t2, ids.pool) %}
    let (claimable) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=t2, memberAddress=account2
    );
    assert claimable = (flowRatePerUnit * u1) * (t2 - timestamp);
    %{ stop_prank_callable = start_prank(ids.account2, ids.pool) %}
    ISuperTokenPool.claimAll(contract_address=pool);
    let (claimable) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=t2, memberAddress=account2
    );
    assert claimable = 0;
    ISuperTokenPool.claimAll(contract_address=pool);
    let (claimable) = ISuperTokenPool.getClaimable(
        contract_address=pool, time=t2, memberAddress=account2
    );
    assert claimable = 0;
    %{ stop_prank_callable() %}
    %{ stop_warp() %}
    return ();
}
