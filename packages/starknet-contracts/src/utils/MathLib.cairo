%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem, signed_div_rem, assert_in_range
from starkware.cairo.common.math_cmp import RC_BOUND, is_le, is_not_zero, is_nn
from starkware.cairo.common.bool import TRUE


const DIVISOR_MAX = 10633823966279327296825105735305134080; // PRIME//RC_BOUND

const BOUND = RC_BOUND/2; // bound <= rc_bound / 2.

namespace MathLib {

    func div{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(value: felt, div: felt) -> (quotient: felt, remainder: felt) {
        alloc_locals;
        with_attr error_message("MathLib: Invalid Divisor") {
            assert_in_range(div, 0, DIVISOR_MAX);
        }
        let is_not_negative = is_nn(value);
        if (is_not_negative == TRUE){
            let (quotient, remainder) = unsigned_div_rem(value, div);
            return (quotient=quotient, remainder=remainder);
        } else {
            let (quotient, remainder) = signed_div_rem(value, div, BOUND);
            return (quotient=quotient, remainder=remainder);
        }
    }
}
