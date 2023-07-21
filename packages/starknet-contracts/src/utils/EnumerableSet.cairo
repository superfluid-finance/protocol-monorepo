%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.bool import TRUE, FALSE

struct Set {
    values: felt*,
    indexes: felt*,
    length: felt,
}

namespace EnumerableSet {
    func add{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        set: Set, value: felt
    ) -> (set: Set, success: felt) {
        let setContains = contains(set, value);
        if (setContains == TRUE) {
            assert [set.values + set.length] = value;
            assert [set.indexes] = set.length;
            let new_set = Set(set.values, set.indexes, set.length + 1);
            return (set=new_set, success=TRUE);
        }
        return (set=Set(0, 0, 0), success=FALSE);
    }

    func contains{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        set: Set, value: felt
    ) -> (success: felt) {
        let found = find(set, value);
        return (success=found);
    }

    func find{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        set: Set, value: felt
    ) -> (success: felt) {
        if (set.length != 0) {
            let _value = [set.values + (set.length - 1)];
            if (_value == value) {
                return (success=TRUE);
            } else {
                let new_set = Set(set.indexes, set.values, set.length - 1);
                find(new_set, value);
            }
        }
        return (success=FALSE);
    }
}
