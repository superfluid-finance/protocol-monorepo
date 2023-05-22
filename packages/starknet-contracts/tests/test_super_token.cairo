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
        context.supertoken_contract_address = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", [1539470638642759296633, 21332, 18, 2961438646427897227265458639523639022044227144684039946536102553631442914981]).contract_address
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
    %{ stop_prank_callable = start_prank(ids.by, ids.supertoken_contract_address) %}
    ISuperfluidToken.disconnectPool(contract_address=supertoken_contract_address, to=pool);
    %{ stop_prank_callable() %}
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
    %{   
        ids.supertoken_contract_address = context.supertoken_contract_address   
        ids.test_account_address1 = context.test_account_address1
        ids.test_account_address2 = context.test_account_address2
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

    %{ stop_warp() %}

    return ();
}

// @external
// func setup_distribute1to2flow_one_connected{
//     syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
// }() {
//     %{
//         given(
//             fr = strategy.integers(1, 1000000),
//             u1 = strategy.integers(0, 1000000),
//             u2 = strategy.integers(0, 1000000),
//             account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             t1 = strategy.integers(1, 100),
//         )
//     %}
//     return ();
// }

// @external
// func test_distribute1to2flow_one_connected{
//     syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
// }(fr: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt, t1: felt) {
//     alloc_locals;
//     %{ assume(ids.account1 != ids.account2) %}
//     %{ assume(ids.account2 != ids.account3) %}
//     %{ assume(ids.account1 != ids.account3) %}
//     let tu = u1 + u2;
//     %{ assume(ids.tu > 0) %}

// tempvar supertoken_contract_address;
//     %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     let (pool) = ISuperfluidToken.createPool(contract_address=supertoken_contract_address);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
//     ISuperfluidPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
//     ISuperfluidPool.updateMember(contract_address=pool, memberAddress=account3, unit=u2);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     ISuperfluidToken.distributeFlow(
//         contract_address=supertoken_contract_address,
//         senderAddress=account1,
//         poolAddress=pool,
//         flowId=0,
//         reqFlowRate=fr,
//     );
//     %{ stop_prank_callable() %}

// let (pendingUnits) = ISuperfluidPool.getPendingUnits(contract_address=pool);
//     assert pendingUnits = u1 + u2;

// %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
//     ISuperfluidToken.connectPool(contract_address=supertoken_contract_address, to=pool);
//     %{ stop_prank_callable() %}

// let (pendingUnits) = ISuperfluidPool.getPendingUnits(contract_address=pool);
//     assert pendingUnits = u2;

// let (timestamp) = get_block_timestamp();
//     let t2 = t1 + timestamp;

// let tu = u1 + u2;
//     let (quotient, _) = unsigned_div_rem(fr, tu);
//     let actualFlowRate = quotient * tu;
//     let flowRatePerUnit = actualFlowRate / tu;

// %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}

// let (distributionFlowRate) = ISuperfluidPool.getDistributionFlowRate(contract_address=pool);
//     let (pendingDistributionFlowRate) = ISuperfluidPool.getPendingDistributionFlowRate(
//         contract_address=pool
//     );

// let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account3
//     );
//     assert distributionFlowRate = actualFlowRate;
//     assert netFlowRateOfAccount1 = -actualFlowRate;
//     assert netFlowRateOfAccount2 = flowRatePerUnit * u1;
//     assert netFlowRateOfAccount3 = 0;
//     assert netFlowRateOfAccount2 + pendingDistributionFlowRate = distributionFlowRate;
//     assert netFlowRateOfAccount1 + netFlowRateOfAccount2 + pendingDistributionFlowRate = 0;

// let (balanceOfAccount1) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     let (balanceOfAccount2) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (balanceOfAccount3) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account3
//     );
//     // The value should be -((t2 - timestamp) * actualFlowRate) but negative values are represented as 0
//     assert balanceOfAccount1 = 0;
//     assert balanceOfAccount2 = (t2 - timestamp) * netFlowRateOfAccount2;
//     assert balanceOfAccount3 = 0;
//     %{ stop_warp() %}
//     return ();
// }

// @external
// func setup_distribute2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
//     %{
//         given(
//             fr = strategy.integers(1, 1000000),
//             u1 = strategy.integers(0, 1000000),
//             u2 = strategy.integers(0, 1000000),
//             account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             account3 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             t1 = strategy.integers(1, 100),
//         )
//     %}
//     return ();
// }

// @external
// func test_distribute2to1_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     fr: felt, u1: felt, u2: felt, account1: felt, account2: felt, account3: felt, t1: felt
// ) {
//     alloc_locals;
//     %{ assume(ids.account1 != ids.account2) %}
//     %{ assume(ids.account2 != ids.account3) %}
//     %{ assume(ids.account1 != ids.account3) %}
//     %{ assume(ids.u1 > 0) %}
//     %{ assume(ids.u2 > 0) %}

// tempvar supertoken_contract_address;
//     %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     let (pool1) = ISuperfluidToken.createPool(contract_address=supertoken_contract_address);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
//     let (pool2) = ISuperfluidToken.createPool(contract_address=supertoken_contract_address);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.pool1) %}
//     ISuperfluidPool.updateMember(contract_address=pool1, memberAddress=account3, unit=u1);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account2, ids.pool2) %}
//     ISuperfluidPool.updateMember(contract_address=pool2, memberAddress=account3, unit=u2);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     ISuperfluidToken.distributeFlow(
//         contract_address=supertoken_contract_address,
//         senderAddress=account1,
//         poolAddress=pool1,
//         flowId=0,
//         reqFlowRate=fr,
//     );
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
//     ISuperfluidToken.distributeFlow(
//         contract_address=supertoken_contract_address,
//         senderAddress=account2,
//         poolAddress=pool2,
//         flowId=0,
//         reqFlowRate=fr,
//     );
//     %{ stop_prank_callable() %}

// let (pendingUnitsPool1) = ISuperfluidPool.getPendingUnits(contract_address=pool1);
//     assert pendingUnitsPool1 = u1;

// let (pendingUnitsPool2) = ISuperfluidPool.getPendingUnits(contract_address=pool2);
//     assert pendingUnitsPool2 = u2;

// %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
//     ISuperfluidToken.connectPool(contract_address=supertoken_contract_address, to=pool1);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account3, ids.supertoken_contract_address) %}
//     ISuperfluidToken.connectPool(contract_address=supertoken_contract_address, to=pool2);
//     %{ stop_prank_callable() %}

// let (pendingUnitsPool1) = ISuperfluidPool.getPendingUnits(contract_address=pool1);
//     assert pendingUnitsPool1 = 0;

// let (pendingUnitsPool2) = ISuperfluidPool.getPendingUnits(contract_address=pool2);
//     assert pendingUnitsPool2 = 0;

// let (timestamp) = get_block_timestamp();
//     let t2 = t1 + timestamp;

// let (quotientForPool1, _) = unsigned_div_rem(fr, u1);
//     let actualFlowRateForPool1 = quotientForPool1 * u1;
//     let flowRatePerUnitForPool1 = actualFlowRateForPool1 / u1;

// let (quotientForPool2, _) = unsigned_div_rem(fr, u2);
//     let actualFlowRateForPool2 = quotientForPool2 * u2;
//     let flowRatePerUnitForPool2 = actualFlowRateForPool2 / u2;

// %{ stop_warp = warp(ids.t2, ids.supertoken_contract_address) %}
//     let (distributionFlowRateForPool1) = ISuperfluidPool.getDistributionFlowRate(
//         contract_address=pool1
//     );
//     let (distributionFlowRateForPool2) = ISuperfluidPool.getDistributionFlowRate(
//         contract_address=pool2
//     );

// let (pendingDistributionFlowRateForPool1) = ISuperfluidPool.getPendingDistributionFlowRate(
//         contract_address=pool1
//     );
//     let (pendingDistributionFlowRateForPool2) = ISuperfluidPool.getPendingDistributionFlowRate(
//         contract_address=pool2
//     );

// let (netFlowRateOfAccount1) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     let (netFlowRateOfAccount2) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (netFlowRateOfAccount3) = ISuperfluidToken.getNetFlowRate(
//         contract_address=supertoken_contract_address, account=account3
//     );

// assert distributionFlowRateForPool1 = actualFlowRateForPool1;
//     assert distributionFlowRateForPool2 = actualFlowRateForPool2;

// assert netFlowRateOfAccount1 = -actualFlowRateForPool1;
//     assert netFlowRateOfAccount2 = -actualFlowRateForPool2;

// assert netFlowRateOfAccount3 = (
//         (flowRatePerUnitForPool1 * u1) + (flowRatePerUnitForPool2 * u2)
//     );

// assert netFlowRateOfAccount1 + netFlowRateOfAccount2 + netFlowRateOfAccount3 = 0;

// let (balanceOfAccount1) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     let (balanceOfAccount2) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     let (balanceOfAccount3) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account3
//     );
//     // The value should be -((t2 - timestamp) * actualFlowRate) but negative values are represented as 0
//     assert balanceOfAccount1 = 0;
//     assert balanceOfAccount2 = 0;
//     assert balanceOfAccount3 = (t2 - timestamp) * netFlowRateOfAccount3;
//     %{ stop_warp() %}
//     return ();
// }

// @external
// func setup_pool_multiple_claims{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
//     %{
//         given(
//             fr = strategy.integers(1, 1000000),
//             u1 = strategy.integers(0, 1000000),
//             account1 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             account2 = strategy.integers(1, 100000000000000000000000000000000000000000000000000000000000000000000000),
//             t1 = strategy.integers(1, 1000),
//         )
//     %}
//     return ();
// }

// @external
// func test_pool_multiple_claims{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     fr: felt, u1: felt, account1: felt, account2: felt, t1: felt
// ) {
//     alloc_locals;
//     %{ assume(ids.account1 != ids.account2) %}
//     %{ assume(ids.u1 > 0) %}

// tempvar supertoken_contract_address;
//     %{ ids.supertoken_contract_address = context.supertoken_contract_address %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     let (pool) = ISuperfluidToken.createPool(contract_address=supertoken_contract_address);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.pool) %}
//     ISuperfluidPool.updateMember(contract_address=pool, memberAddress=account2, unit=u1);
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account1, ids.supertoken_contract_address) %}
//     ISuperfluidToken.distributeFlow(
//         contract_address=supertoken_contract_address,
//         senderAddress=account1,
//         poolAddress=pool,
//         flowId=0,
//         reqFlowRate=fr,
//     );
//     %{ stop_prank_callable() %}

// %{ stop_prank_callable = start_prank(ids.account2, ids.supertoken_contract_address) %}
//     ISuperfluidToken.connectPool(contract_address=supertoken_contract_address, to=pool);
//     %{ stop_prank_callable() %}

// let (timestamp) = get_block_timestamp();
//     let t2 = t1 + timestamp;

// let (quotient, _) = unsigned_div_rem(fr, u1);
//     let actualFlowRate = quotient * u1;
//     let flowRatePerUnit = actualFlowRate / u1;

// let (balanceOfAccount1) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account1
//     );
//     assert balanceOfAccount1 = 0;
//     let (balanceOfAccount2) = ISuperfluidToken.balanceOf(
//         contract_address=supertoken_contract_address, account=account2
//     );
//     assert balanceOfAccount2 = 0;

// %{ stop_warp = warp(ids.t2, ids.pool) %}
//     let (claimable) = ISuperfluidPool.getClaimable(
//         contract_address=pool, time=t2, memberAddress=account2
//     );
//     assert claimable = (flowRatePerUnit * u1) * (t2 - timestamp);
//     %{ stop_prank_callable = start_prank(ids.account2, ids.pool) %}
//     ISuperfluidPool.claimAll(contract_address=pool);
//     let (claimable) = ISuperfluidPool.getClaimable(
//         contract_address=pool, time=t2, memberAddress=account2
//     );
//     assert claimable = 0;
//     ISuperfluidPool.claimAll(contract_address=pool);
//     let (claimable) = ISuperfluidPool.getClaimable(
//         contract_address=pool, time=t2, memberAddress=account2
//     );
//     assert claimable = 0;
//     %{ stop_prank_callable() %}
//     %{ stop_warp() %}
//     return ();
// }
