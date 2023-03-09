// %lang starknet

// from starkware.cairo.common.cairo_builtins import HashBuiltin

// struct Data {
//     key: felt;
//     value: felt;
// }

// namespace BitMap {

// func set{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//         set: Set, value: felt
//     ) -> (data: Data, index: felt) {
//         let bucket = index / (251); // Right-Shift
//         let mask = 1 * (2 ** (index / (2 ** 251)));
//     }

// }
