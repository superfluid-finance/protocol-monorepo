// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

/**
 * @title Int96SafeMath
 * @dev Int96 math operations with safety checks that revert on error.
 */
library Int96SafeMath {
    int96 constant private _INT96_MIN = -2**95;

    /**
     * @dev Returns the multiplication of two signed integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `*` operator.
     *
     * Requirements:
     *
     * - Multiplication cannot overflow.
     */
    function mul(int96 a, int96 b, string memory errorMessage) internal pure returns (int96) {
        // Gas optimization: this is cheaper than requiring 'a' not being zero, but the
        // benefit is lost if 'b' is also tested.
        // See: https://github.com/OpenZeppelin/openzeppelin-contracts/pull/522
        if (a == 0) {
            return 0;
        }

        require(!(a == -1 && b == _INT96_MIN), errorMessage);
        int96 c;
        
        unchecked {
            c = a * b;
        }
        
        require(c / a == b, errorMessage);

        return c;
    }

    /**
     * @dev Returns the integer division of two signed integers. Reverts on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     *
     * - The divisor cannot be zero.
     */
    function div(int96 a, int96 b, string memory errorMessage) internal pure returns (int96) {
        require(b != 0, errorMessage);
        require(!(b == -1 && a == _INT96_MIN), errorMessage);

        int96 c = a / b;

        return c;
    }

    /**
     * @dev Returns the subtraction of two signed integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     *
     * - Subtraction cannot overflow.
     */
    function sub(int96 a, int96 b, string memory errorMessage) internal pure returns (int96) {
        int96 c;

        unchecked {
            c = a - b;
        }

        require((b >= 0 && c <= a) || (b < 0 && c > a), errorMessage);

        return c;
    }

    /**
     * @dev Returns the addition of two signed integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `+` operator.
     *
     * Requirements:
     *
     * - Addition cannot overflow.
     */
    function add(int96 a, int96 b, string memory errorMessage) internal pure returns (int96) {
        int96 c;
        
        unchecked {
            c = a + b;
        }

        require((b >= 0 && c >= a) || (b < 0 && c < a), errorMessage);

        return c;
    }
}
