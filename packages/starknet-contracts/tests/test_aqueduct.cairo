%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.interfaces.ISuperfluidToken import ISuperfluidToken
from src.interfaces.IAqueduct import IAqueduct

// @external
// func __setup__{syscall_ptr: felt*}() {
//     %{
//                 declare("./src/pools/PoolImpl.cairo")
//                 context.supertoken_1 = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", 
//         [1539470638642759296633, 21332, 18, 
//         1967013752834806001269811315755539563695215919214241724661593146835538551452]).contract_address
//                 context.supertoken_2 = deploy_contract("./src/tokens/ERC20x/SuperToken/SuperTokenImpl.cairo", 
//         [1539470638642759296633, 21332, 18, 
//         1967013752834806001269811315755539563695215919214241724661593146835538551452]).contract_address
//                 context.aqueduct = deploy_contract("./src/examples/aqueduct/AqueductImpl.cairo", [context.supertoken_1, 
//         context.supertoken_2]).contract_address
//     %}
//     return ();
// }

// @external
// func _flowWithCallback{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     token: felt, _from: felt, rate: felt
// ) {
//     tempvar aqueduct;
//     %{ ids.aqueduct = context.aqueduct %}

//     %{ stop_prank_callable = start_prank(ids._from, ids.token) %}
//     let (oldRate) = ISuperToken.getFlowRate(
//         contract_address=token, _from=_from, to=aqueduct, flowId=0
//     );
//     ISuperToken.flow(
//         contract_address=token,
//         senderAddress=_from,
//         receiverAddress=aqueduct,
//         flowId=0,
//         flowRate=rate,
//     );
//     IAqueduct.onFlowUpdate(
//         contract_address=aqueduct, token=token, _from=_from, ir0=oldRate, ir1=rate
//     );
//     %{ stop_prank_callable() %}
//     return ();
// }

// @external
// func setup_1lp_bootstrap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
//     %{
//         given(
//             r1 = strategy.felts(), # first rate
//             r2 = strategy.felts(), # second rate
//             t1 = strategy.integers(1, 1000), # time
//         )
//     %}
//     return ();
// }

// @external
// func test_1lp_bootstrap{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     r1: felt, r2: felt, t1: felt
// ) {
//     return ();
// }
