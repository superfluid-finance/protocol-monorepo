%lang starknet

@storage_var
func Test_name() -> (res: felt) {
}

from starkware.cairo.common.cairo_builtins import HashBuiltin

@view
func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
    let (name) = Test_name.read();
    return (name=name);
}

@external
func setName{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    name: felt
) {
    Test_name.write(name);
    return ();
}