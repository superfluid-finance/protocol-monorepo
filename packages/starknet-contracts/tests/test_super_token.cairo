%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_contract_address,
    get_block_timestamp,
    get_tx_info,
)
from starkware.cairo.common.math import unsigned_div_rem
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.alloc import alloc

from src.tokens.ERC20x.SuperToken.library import SuperToken
from src.utils.SemanticMoney import (
    SemanticMoney,
    BasicParticle,
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
)
from src.utils.account.ITestAccount import ITestAccount
from src.utils.account.library import Call
from src.interfaces.ISuperfluidToken import ISuperfluidToken
from src.interfaces.ISuperfluidPool import ISuperfluidPool, ISuperfluidPoolOperator

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
    %{
        declare("./src/pools/PoolImpl.cairo")
        context.test_account_address1 = deploy_contract("./src/utils/account/TestAccountImpl.cairo").contract_address
        context.test_account_address2 = deploy_contract("./src/utils/account/TestAccountImpl.cairo").contract_address
        context.supertoken_contract_address = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", [1539470638642759296633, 21332, 18, 3600748221340956293245257266815110776189556141392786261445796616585040799092]).contract_address
        context.MINT_AMOUNT = 1000000000000000000
        context.LIQUIDATION_PERIOD = 1000
    %}
    return ();
}


func _createPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(by: felt) -> (pool: felt) {
    tempvar supertoken_contract_address;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
    %}
    %{ stop_prank_callable = start_prank(ids.by, ids.supertoken_contract_address) %}
    let (pool) = ISuperfluidToken.createPool(contract_address=supertoken_contract_address);
    %{ stop_prank_callable() %}
    return (pool=pool);
}

func _connectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt, by: felt) {
    alloc_locals;
    tempvar supertoken_contract_address;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
    %}
    let (local calls: Call*) = alloc();
    let connectPoolSelector = 548208072097493527300120807205932284557878311980095480278893301204690846585;
    assert [calls] = Call(supertoken_contract_address, connectPoolSelector);

    let (local calldata: felt*) = alloc();
    assert [calldata] = pool;
    ITestAccount.execute(contract_address=by, calls_len=1, calls=calls, calldata_len=1, calldata=calldata);
    return ();
}

func _disconnectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt, by: felt) {
    alloc_locals;
    tempvar supertoken_contract_address;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
    %}
    let (local calls: Call*) = alloc();
    let disconnectPoolSelector = 28166688197912724383049707936424227093755017102822508708685168553869669375;
    assert [calls] = Call(supertoken_contract_address, disconnectPoolSelector);

    let (local calldata: felt*) = alloc();
    assert [calldata] = pool;
    ITestAccount.execute(contract_address=by, calls_len=1, calls=calls, calldata_len=1, calldata=calldata);
    return ();
}

func _getAdjustmentFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt, expectedRecipient: felt) -> (flowRate: felt) {
    tempvar supertoken_contract_address;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
    %}
    let (recipient,_,flowRate) = ISuperfluidToken.getPoolAdjustmentFlowInfo(contract_address=supertoken_contract_address, pool=pool);
    with_attr error_message("SuperToken Test: expectedRecipient fail") {
        assert recipient = expectedRecipient;
    }
    return (flowRate=flowRate);
}

@external
func setup_erc20_transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            x1 = strategy.integers(1, 1000000),
            x2 = strategy.integers(1, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_erc20_transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(x1: felt, x2: felt, account1: felt, account2: felt) {
    %{ assume(ids.account1 != ids.account2) %}
    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account2, amount=MINT_AMOUNT);
    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.transfer(contract_address=supertoken_contract_address, to=account2, amount=x1);
    %{ stop_prank_callable() %}
    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperfluidToken.transfer(contract_address=supertoken_contract_address, to=account1, amount=x2);
    %{ stop_prank_callable() %}
    let (balanceOfAccount1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (balanceOfAccount2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account2);
    assert balanceOfAccount1 = MINT_AMOUNT - x1 + x2;
    assert balanceOfAccount2 = MINT_AMOUNT - x2 + x1;
    return ();
}

@external
func setup_erc20_self_transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            x = strategy.integers(1, 1000000),
            account = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_erc20_self_transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(x: felt, account: felt) {
    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account, amount=MINT_AMOUNT);
    %{ stop_prank_callable = start_prank(ids.account, ids.supertoken_contract_address) %}
    ISuperfluidToken.transfer(contract_address=supertoken_contract_address, to=account, amount=x);
    %{ stop_prank_callable() %}
    let (balanceOfAccount) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account);
    assert balanceOfAccount = MINT_AMOUNT;
    return ();
}

@external
func setup_1to1_flow_update{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr1 = strategy.integers(1, 1000000),
            fr2 = strategy.integers(1, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            dt1 = strategy.integers(1, 1000),
            dt2 = strategy.integers(1, 1000)
        )
    %}
    return ();
}

@external
func test_1to1_flow_update{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, dt1: felt, dt2: felt
) {
    %{ assume(ids.account1 != ids.account2) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    tempvar LIQUIDATION_PERIOD;
    %{ 
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );

    assert netFlowRateOfAccount1 = 0;
    assert netFlowRateOfAccount2 = 0;

    let (flowRateOfAccount1andAccount2) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = 0;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account2,
        flowId=0,
        flowRate=fr1,
    );
    %{ stop_prank_callable() %}

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );

    assert netFlowRateOfAccount1 = -fr1;
    assert netFlowRateOfAccount2 = fr1;

    let (flowRateOfAccount1andAccount2) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = fr1;

    let (timestamp) = get_block_timestamp();
    let t1 = timestamp + dt1;
    let t2 = t1 + dt2;
    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}
    let (balanceOfSender) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfReceiver) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let bufferAmount = LIQUIDATION_PERIOD * fr1;
    assert balanceOfSender = MINT_AMOUNT - (fr1 * (t1 - timestamp)) - bufferAmount;
    assert balanceOfReceiver = (fr1 * (t1 - timestamp));
    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account2,
        flowId=0,
        flowRate=fr2,
    );
    %{ stop_prank_callable() %}

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );

    assert netFlowRateOfAccount1 = -fr2;
    assert netFlowRateOfAccount2 = fr2;

    let (flowRateOfAccount1andAccount2) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = fr2;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    let (balanceOfSender_1) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfReceiver_1) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let newBufferAmount = LIQUIDATION_PERIOD * fr2;
    let bufferDelta = newBufferAmount - bufferAmount;
    assert balanceOfSender_1 = balanceOfSender - (fr2 * (t2 - t1)) - bufferDelta;
    assert balanceOfReceiver_1 = balanceOfReceiver + (fr2 * (t2 - t1));
    %{ stop_warp() %}
    return ();
}

@external
func setup_1to2_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            fr1 = strategy.integers(1, 100000000),
            fr2 = strategy.integers(1, 100000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            t1 = strategy.integers(1, 1000)
        )
    %}
    return ();
}

@external
func test_1to2_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt, t1: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    tempvar LIQUIDATION_PERIOD;
    %{ 
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = 0;
    assert netFlowRateOfAccount2 = 0;
    assert netFlowRateOfAccount3 = 0;

    let (flowRateOfAccount1andAccount2) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );
    let (flowRateOfAccount1andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = 0;
    assert flowRateOfAccount1andAccount3 = 0;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account2,
        flowId=0,
        flowRate=fr1,
    );
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr2,
    );
    %{ stop_prank_callable() %}

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = -(fr1 + fr2);
    assert netFlowRateOfAccount2 = fr1;
    assert netFlowRateOfAccount3 = fr2;

    let (flowRateOfAccount1andAccount2) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account2, flowId=0
    );
    let (flowRateOfAccount1andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount2 = fr1;
    assert flowRateOfAccount1andAccount3 = fr2;

    let (timestamp) = get_block_timestamp();
    let t2 = timestamp + t1;
    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    let (balanceOfSender) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfReceiver1) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfReceiver2) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    %{ stop_warp() %}
    assert balanceOfSender = MINT_AMOUNT - ((fr1 + fr2) * (t2 - timestamp)) - (LIQUIDATION_PERIOD * (fr1 + fr2));
    assert balanceOfReceiver1 = (fr1 * (t2 - timestamp));
    assert balanceOfReceiver2 = (fr2 * (t2 - timestamp));
    return ();
}

@external
func setup_2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
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
func test_2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    fr1: felt, fr2: felt, account1: felt, account2: felt, account3: felt, t1: felt
) {
    %{ assume(ids.account1 != ids.account2) %}
    %{ assume(ids.account1 != ids.account3) %}
    %{ assume(ids.account2 != ids.account3) %}

    tempvar supertoken_contract_address;
    %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = 0;
    assert netFlowRateOfAccount2 = 0;
    assert netFlowRateOfAccount3 = 0;

    let (flowRateOfAccount1andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );
    let (flowRateOfAccount2andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account2, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount3 = 0;
    assert flowRateOfAccount2andAccount3 = 0;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr1,
    );
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    ISuperfluidToken.flow(
        contract_address=supertoken_contract_address,
        senderAddress=account2,
        receiverAddress=account3,
        flowId=0,
        flowRate=fr2,
    );
    %{ stop_prank_callable() %}

    let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account1
    );
    let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account2
    );
    let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
        contract_address=supertoken_contract_address, account=account3
    );

    assert netFlowRateOfAccount1 = -fr1;
    assert netFlowRateOfAccount2 = -fr2;
    assert netFlowRateOfAccount3 = fr1 + fr2;

    let (flowRateOfAccount1andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account1, to=account3, flowId=0
    );
    let (flowRateOfAccount2andAccount3) = ISuperfluidToken.getFlowRate(
        contract_address=supertoken_contract_address, _from=account2, to=account3, flowId=0
    );

    assert flowRateOfAccount1andAccount3 = fr1;
    assert flowRateOfAccount2andAccount3 = fr2;

    let (timestamp) = get_block_timestamp();
    let t2 = t1 + timestamp;
    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    let (balanceOfAccount1) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account2
    );
    let (balanceOfAccount3) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account3
    );
    %{ stop_warp() %}
    assert balanceOfAccount1 = 0;
    assert balanceOfAccount2 = 0;
    assert balanceOfAccount3 = ((fr2 + fr1) * (t2 - timestamp));
    return ();
}

@external
func setup_1to2_instdistribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            amount = strategy.integers(1, 1000000),
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_1to2_instdistribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    amount: felt, u1: felt, u2: felt, account1: felt
) {
    alloc_locals;
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}

    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;

    let (contract_address) = get_contract_address();

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u1);
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address2, unit=u2);
    %{ stop_prank_callable() %}

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = u1 + u2;

    let tu = u1 + u2;

    let (poolIndex) = ISuperfluidPool.getIndex(contract_address=pool);
    assert poolIndex.total_units = tu;

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distribute(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        reqAmount=amount,
    );
    %{ stop_prank_callable() %}

    let (time) = get_block_timestamp();
    let (claimableForAccount2) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address1);
    let (claimableForAccount3) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address2);
    let (quotient, _) = unsigned_div_rem(amount, tu);
    let actualAmount = quotient * tu;
    let amountPerUnit = actualAmount / tu;
    assert claimableForAccount2 = amountPerUnit * u1;
    assert claimableForAccount3 = amountPerUnit * u2;

    let (isConnectedToPool) = ISuperfluidPoolOperator.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=test_account_address1
    );
    let (connectedPools) = ISuperfluidToken.getNumConnections(
        contract_address=supertoken_contract_address, account=test_account_address1
    );
    assert isConnectedToPool = FALSE;
    assert connectedPools = 0;

    /// Call ConnectPool Using the TestAccount 1
    _connectPool(pool, test_account_address1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}

    let (isConnectedToPool) = ISuperfluidPoolOperator.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=test_account_address1
    );
    let (connectedPools) = ISuperfluidToken.getNumConnections(
        contract_address=supertoken_contract_address, account=test_account_address1
    );
    assert isConnectedToPool = TRUE;
    assert connectedPools = 1;

    let (isConnectedToPool) = ISuperfluidPoolOperator.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=test_account_address2
    );
    let (connectedPools) = ISuperfluidToken.getNumConnections(
        contract_address=supertoken_contract_address, account=test_account_address2
    );
    assert isConnectedToPool = FALSE;
    assert connectedPools = 0;

    /// Calling ConnectPool Using the Test Account 2
    _connectPool(pool, test_account_address2);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}

    let (isConnectedToPool) = ISuperfluidPoolOperator.isMemberConnected(
        contract_address=supertoken_contract_address, pool=pool, memberAddress=test_account_address2
    );
    let (connectedPools) = ISuperfluidToken.getNumConnections(
        contract_address=supertoken_contract_address, account=test_account_address2
    );
    assert isConnectedToPool = TRUE;
    assert connectedPools = 1;

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;

    let (balanceOfAccount1) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=account1
    );
    let (balanceOfAccount2) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=test_account_address1
    );
    let (balanceOfAccount3) = ISuperfluidToken.balanceOf(
        contract_address=supertoken_contract_address, account=test_account_address2
    );
    let (quotient, _) = unsigned_div_rem(amount, tu);
    let actualAmount = quotient * tu;
    let amountPerUnit = actualAmount / tu;
    assert balanceOfAccount1 = MINT_AMOUNT - actualAmount;
    assert balanceOfAccount2 = amountPerUnit * u1;
    assert balanceOfAccount3 = amountPerUnit * u2;
    let (time) = get_block_timestamp();
    let (claimableForAccount2) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address1);
    let (claimableForAccount3) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address2);
    assert claimableForAccount2 = 0;
    assert claimableForAccount3 = 0;
    return ();
}

@external
func setup_1to2_distributeflow_bothconnected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            r1 = strategy.integers(1, 1000000),
            r2 = strategy.integers(1, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_1to2_distributeflow_bothconnected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(u1: felt, u2: felt, r1: felt, r2: felt, dt1: felt, dt2: felt, account1: felt) {
    alloc_locals;
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}
    let (rr1, _) = unsigned_div_rem(r1, tu);
    let rrr1 = rr1 * tu;
    let (rr2, _) = unsigned_div_rem(r2, tu);
    let rrr2 = rr2 * tu;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}
    let (a1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (c1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address2);
    let (p1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u1);
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address2, unit=u2);
    %{ stop_prank_callable() %}
    let (time) = get_block_timestamp();
    let (claimableForAccount2) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address1);
    let (claimableForAccount3) = ISuperfluidPool.getClaimable(contract_address=pool, time=time, memberAddress=test_account_address2);
    assert claimableForAccount2 = 0;
    assert claimableForAccount3 = 0;
    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (_,af,ndf) = ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r1);
    %{ stop_prank_callable() %}

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = tu;
    
    tempvar test_account_address1;
    %{
        ids.test_account_address1 = context.test_account_address1
    %}
    _connectPool(pool, test_account_address1);
    tempvar test_account_address2;
    %{
        ids.test_account_address2 = context.test_account_address2
    %}
    _connectPool(pool, test_account_address2);

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;
    
    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    tempvar LIQUIDATION_PERIOD;
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address   
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    let (pdr1) = ISuperfluidPool.getConnectedFlowRate(contract_address=pool);
    let (adj1) = _getAdjustmentFlowRate(pool, account1);
    let (a1nr1) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account1);
    let (ta1nr1) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address1);
    let (ta2nr1) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address2);
    let (pnr1) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=pool);

    assert pdr1 = ndf;
    assert a1nr1 = -af + adj1;
    assert pdr1 = rrr1;
    assert a1nr1 = -rrr1;
    assert ta1nr1 + ta2nr1 = rrr1;
    assert pnr1 = -adj1;
    assert a1nr1 + ta1nr1 + ta2nr1 + pnr1 = 0;

    let (timestamp) = get_block_timestamp();
    let t1 = timestamp + dt1;
    let t2 = t1 + dt2;
    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (_,af,ndf) = ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r2);
    %{ stop_prank_callable() %}

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    let (pdr2) = ISuperfluidPool.getConnectedFlowRate(contract_address=pool);
    let (adj2) = _getAdjustmentFlowRate(pool, account1);
    let (a1nr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account1);
    let (ta1nr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address1);
    let (ta2nr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address2);
    let (pnr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=pool);
    
    assert pdr2 = ndf;
    assert a1nr2 = -af + adj2;
    assert pdr2 = rrr2;
    assert a1nr2 = -rrr2;
    assert ta1nr2 + ta2nr2 = rrr2;
    assert pnr2 = -adj2;
    assert a1nr2 + ta1nr2 + ta2nr2 + pnr2 = 0;

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (c2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address2);
    let (p2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    %{ stop_warp() %}

    assert a1 - a2 = k2 + (rrr1 * dt1) + (rrr2 * dt2);
    assert a1 - a2 = k2 + (b2 - b1) + (c2 - c1) + (p2 - p1);

    let (claimableForTestAccount1) = ISuperfluidPool.getClaimable(contract_address=pool, time=t2, memberAddress=test_account_address1);
    let (claimableForTestAccount2) = ISuperfluidPool.getClaimable(contract_address=pool, time=t2, memberAddress=test_account_address2);

    assert a1 - a2 - k2 = claimableForTestAccount1 + claimableForTestAccount2;
    assert k2 = rrr2 * LIQUIDATION_PERIOD;

    return ();
}

@external
func setup_1to2_distributeflow_oneconnected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            r = strategy.integers(1, 1000000),
            dt = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_1to2_distributeflow_oneconnected{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}(u1: felt, u2: felt, r: felt, dt: felt, account1: felt) {
    alloc_locals;
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    let (rr, _) = unsigned_div_rem(r, tu);
    let rrr = rr * tu;

    let (timestamp) = get_block_timestamp();
    let t1 = timestamp + dt;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);
    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
    %}
    let (a1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (c1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address2);
    let (p1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u1);
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address2, unit=u2);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=r,
    );
    %{ stop_prank_callable() %}

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = tu;

    tempvar test_account_address1;
    %{
        ids.test_account_address1 = context.test_account_address1
    %}
    _connectPool(pool, test_account_address1);

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = u2;

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar test_account_address2;
    tempvar LIQUIDATION_PERIOD;
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address   
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (c2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address2);
    let (p2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    assert a1 - a2 = k2 + (rrr * dt);
    assert c2 - c1 = 0;
    assert a1 - a2 = k2 + (b2 - b1) + (c2 - c1) + (p2 - p1);

    let (claimableForTestAccount1) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);
    let (claimableForTestAccount2) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address2);

    assert a1 - a2 - k2 = claimableForTestAccount1 + claimableForTestAccount2;
    assert k2 = rrr * LIQUIDATION_PERIOD;

    let (ar2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account1);
    let (br2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address1);
    let (cr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address2);
    let (pr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=pool);

    let (cfr) = ISuperfluidPool.getConnectedFlowRate(contract_address=pool);
    assert cfr = rrr;
    assert ar2 = -rrr;
    let (dfr) = ISuperfluidPool.getDisconnectedFlowRate(contract_address=pool);
    assert br2 + dfr = rrr;
    assert pr2 = dfr;
    assert ar2 + br2 + cr2 + pr2 = 0;
    return ();
}

@external
func setup_1to1_distributeflow_connect_disconnect_connect{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u = strategy.integers(1, 1000000),
            r = strategy.integers(1, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            dt3 = strategy.integers(1, 10000),
            dt4 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_1to1_distributeflow_connect_disconnect_connect{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt, r: felt, dt1: felt, dt2: felt, dt3: felt, dt4: felt, account1: felt) {
    alloc_locals;
    let tu = u;

    let (rr, _) = unsigned_div_rem(r, tu);
    let rrr = rr * tu;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);
    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}
    let (a0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=r,
    );
    %{ stop_prank_callable() %}

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = u;

    let (timestamp) = get_block_timestamp();
    let t0 = timestamp;
    let t1 = t0 + dt1;
    
    // t1
    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}
    tempvar test_account_address1;
    %{
        ids.test_account_address1 = context.test_account_address1
    %}
    _connectPool(pool, test_account_address1);
    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;

    // t2
    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let t2 = t1 + dt2;
    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
    _disconnectPool(pool, test_account_address1);
    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = u;

    // t3
    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let t3 = t2 + dt3;
    %{ stop_warp = warp(ids.t3, ids.supertoken_contract_address) %}
    _connectPool(pool, test_account_address1);
    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;

    // t4
    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar LIQUIDATION_PERIOD;
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    let t4 = t3 + dt4;
    %{ stop_warp = warp(ids.t4, ids.supertoken_contract_address) %}

    let (anr4) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account1);
    let (bnr4) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address1);
    let (pnr4) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=pool);

    let (cfr) = ISuperfluidPool.getConnectedFlowRate(contract_address=pool);

    assert cfr = rrr;
    assert anr4 = -rrr;

    let (dfr) = ISuperfluidPool.getDisconnectedFlowRate(contract_address=pool);
    assert bnr4 + dfr = rrr;
    assert pnr4 = dfr;
    assert anr4 + bnr4 + pnr4 = 0;

    let (a4) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b4) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p4) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k4) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    assert a0 - a4 = k4 + (rrr * (dt1 + dt2 + dt3 + dt4));
    assert a0 - a4 = k4 + (b4 - b0) + (p4 - p0);
    assert k4 = rrr * LIQUIDATION_PERIOD;

    return ();
}

@external
func setup_1to2_distributeflow_unit_updates{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u1 = strategy.integers(0, 1000000),
            u2 = strategy.integers(0, 1000000),
            r1 = strategy.integers(1, 100),
            r2 = strategy.integers(1000, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_1to2_distributeflow_unit_updates{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u1: felt, u2: felt, r1: felt, r2: felt, dt1: felt, dt2: felt, account1: felt) {
    alloc_locals;
    let tu = u1 + u2;
    %{ assume(ids.tu > 0) %}

    let (rr1, _) = unsigned_div_rem(r1, tu);
    let rrr1 = rr1 * tu;
    let (rr2, _) = unsigned_div_rem(r2, tu);
    let rrr2 = rr2 * tu;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);
    let (pool) = _createPool(account1);
    tempvar test_account_address1;
    %{
        ids.test_account_address1 = context.test_account_address1
    %}
    _connectPool(pool, test_account_address1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u1);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=r1,
    );
    %{ stop_prank_callable() %}

    let (timestamp) = get_block_timestamp();
    let t1 = timestamp + dt1;

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u2);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(
        contract_address=supertoken_contract_address,
        senderAddress=account1,
        poolAddress=pool,
        flowId=0,
        reqFlowRate=r2,
    );
    %{ stop_prank_callable() %}

    // let t2 = t1 + dt2;
    // %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    // %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    // ISuperfluidToken.distributeFlow(
    //     contract_address=supertoken_contract_address,
    //     senderAddress=account1,
    //     poolAddress=pool,
    //     flowId=0,
    //     reqFlowRate=1000,
    // );
    // %{ stop_prank_callable() %}

    return ();
}

@external
func setup_2to1_distributeflow{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u = strategy.integers(1, 1000000),
            r1 = strategy.integers(1, 1000000),
            r2 = strategy.integers(1, 1000000),
            dt = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
            account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_2to1_distributeflow{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt, r1: felt, r2: felt, dt: felt, account1: felt, account2: felt) {
    alloc_locals;
    %{ assume(ids.account1 != ids.account2) %}
    let tu = u;
    let (rr1, _) = unsigned_div_rem(r1, tu);
    let rrr1 = rr1 * tu;
    let (rr2, _) = unsigned_div_rem(r2, tu);
    let rrr2 = rr2 * tu;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account2, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let (a1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account2);
    let (c1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    let (_,ar,_) = ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r1);
    let (k1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
    let (_,br,pdr) = ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account2, poolAddress=pool, flowId=0, reqFlowRate=r2);
    let (newBalance) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);
    let k2 = newBalance - k1;
    %{ stop_prank_callable() %}

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = tu;

    _connectPool(pool, test_account_address1);

    let (disconnectedUnits) = ISuperfluidPool.getDisconnectedUnits(contract_address=pool);
    assert disconnectedUnits = 0;

    let (timestamp) = get_block_timestamp();
    let t1 = timestamp + dt;

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    tempvar LIQUIDATION_PERIOD;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
        ids.LIQUIDATION_PERIOD = context.LIQUIDATION_PERIOD
    %}

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    let (ar2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account1);
    let (br2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=account2);
    let (cr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=test_account_address1);
    let (pr2) = ISuperfluidToken.getNetFlowRate(contract_address=supertoken_contract_address, account=pool);
    let (cfr2) = ISuperfluidPool.getConnectedFlowRate(contract_address=pool);
    let (dfr2) = ISuperfluidPool.getDisconnectedFlowRate(contract_address=pool);

    assert cfr2 = rrr1 + rrr2;
    assert dfr2 = 0;
    assert ar2 = -rrr1;
    assert br2 = -rrr2;
    assert pr2 = 0;
    assert ar2 + br2 + cr2 + pr2 = 0;
    assert cfr2 = pdr;
    assert -ar2 = ar;
    assert -br2 = br;

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account2);
    let (c2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    assert a1 - a2 = k1 + (rrr1 * dt);
    assert b1 - b2 = k2 + (rrr2 * dt);
    assert (a1 - a2) + (b1 - b2) = k1 + k2 + (c2 - c1) + (p2 - p1);

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);

    assert (a1 - a2) + (b1 - b2) - k1 - k2 = claimableForTestAccount;
    assert k1 = rrr1 * LIQUIDATION_PERIOD;
    assert k2 = rrr2 * LIQUIDATION_PERIOD;

    return ();
}

@external
func setup_pool_distributeflow_claim_connected_pool{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u = strategy.integers(1, 1000000),
            r = strategy.integers(1, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_pool_distributeflow_claim_connected_pool{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt, r: felt, dt1: felt, dt2: felt, account1: felt) {
    alloc_locals;
    let tu = u;
    let (rr, _) = unsigned_div_rem(r, tu);
    let rrr = rr * tu;
    let (t0) = get_block_timestamp();
    let t1 = t0 + dt1;
    let t2 = t1 + dt2;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let (a0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r);
    %{ stop_prank_callable() %}

    _connectPool(pool, test_account_address1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}
    
    %{ stop_prank_callable = start_prank(ids.test_account_address1, ids.pool) %}
    %{ stop_warp = warp(ids.t1, ids.pool) %}
    ISuperfluidPool.claimAll(contract_address=pool);
    %{ stop_warp() %}
    %{ stop_prank_callable() %}

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);

    assert claimableForTestAccount = 0;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    assert a0 - a2 = k2 + (rrr * (dt1 + dt2));
    assert a0 - a2 = k2 + (b2 - b0) + (p2 - p0);

    return ();
}

@external
func setup_pool_distributeflow_update_unit{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u1 = strategy.integers(1, 1000000),
            u2 = strategy.integers(1, 1000000),
            r = strategy.integers(1, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_pool_distributeflow_update_unit{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u1: felt, u2: felt, r: felt, dt1: felt, dt2: felt, account1: felt) {
    alloc_locals;
    let (rr1, _) = unsigned_div_rem(r, u1);
    let rrr1 = rr1 * u1;
    let (rr2, _) = unsigned_div_rem(rrr1, u2);
    let rrr2 = rr2 * u2;
    let (t0) = get_block_timestamp();
    let t1 = t0 + dt1;
    let t2 = t1 + dt2;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let (a0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p0) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u1);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r);
    %{ stop_prank_callable() %}

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    %{ stop_warp = warp(ids.t1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u2);
    %{ stop_warp() %}
    %{ stop_prank_callable() %}

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);
    assert claimableForTestAccount = 0;

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    assert a0 - a2 = k2 + (rrr1 * dt1) + (rrr2 * dt2);
    assert a0 - a2 = k2 + (b2 - b0) + (p2 - p0);

    return ();
}

@external
func setup_pool_multiple_claims{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
}() {
    %{
        given(
            u = strategy.integers(1, 1000000),
            r = strategy.integers(1, 1000000),
            dt1 = strategy.integers(1, 10000),
            dt2 = strategy.integers(1, 10000),
            account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
        )
    %}
    return ();
}

@external
func test_pool_multiple_claims{
    syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(u: felt, r: felt, dt1: felt, dt2: felt, account1: felt) {
    alloc_locals;
    let (rr, _) = unsigned_div_rem(r, u);
    let rrr = rr * u;
    let (t0) = get_block_timestamp();
    let t1 = t0 + dt1;
    let t2 = t1 + dt2;

    tempvar supertoken_contract_address;
    tempvar MINT_AMOUNT;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.MINT_AMOUNT = context.MINT_AMOUNT
    %}
    ISuperfluidToken.mint(contract_address=supertoken_contract_address, receiver=account1, amount=MINT_AMOUNT);

    let (pool) = _createPool(account1);

    tempvar supertoken_contract_address;
    tempvar test_account_address1;
    %{
        ids.supertoken_contract_address = context.supertoken_contract_address
        ids.test_account_address1 = context.test_account_address1
    %}

    let (a1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p1) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);

    %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
    ISuperfluidPool.updateMember(contract_address=pool, memberAddress=test_account_address1, unit=u);
    %{ stop_prank_callable() %}

    %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
    ISuperfluidToken.distributeFlow(contract_address=supertoken_contract_address, senderAddress=account1, poolAddress=pool, flowId=0, reqFlowRate=r);
    %{ stop_prank_callable() %}

    %{ stop_warp = warp(ids.t1, ids.supertoken_contract_address) %}

    let (a2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (k2) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    let (claimableForAccount1) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=account1);
    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);

    assert a1 - a2 - k2 = claimableForAccount1 + claimableForTestAccount;
    assert b2 = b1;

    %{ stop_prank_callable = start_prank(ids.test_account_address1, ids.pool) %}
    %{ stop_warp = warp(ids.t1, ids.pool) %}

    ISuperfluidPool.claimAll(contract_address=pool);

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);
    assert claimableForTestAccount = 0;

    ISuperfluidPool.claimAll(contract_address=pool);

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t1, memberAddress=test_account_address1);
    assert claimableForTestAccount = 0;

    %{ stop_warp() %}
    %{ stop_prank_callable() %}

    %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

    let (a3) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=account1);
    let (b3) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=test_account_address1);
    let (p3) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=pool);
    let (k3) = ISuperfluidToken.realtimeBalanceNow(contract_address=supertoken_contract_address, account=supertoken_contract_address);

    let (claimableForTestAccount) = ISuperfluidPool.getClaimable(contract_address=pool, time=t2, memberAddress=test_account_address1);

    assert k2 = k3;
    assert a2 - a3 = claimableForTestAccount;
    assert p3 = claimableForTestAccount;

    assert a1 - a3 = k2 + (rrr * (dt1 + dt2));
    assert a1 - a3 = k2 + (b3 - b1) + (p3 - p1);

    return ();
}
