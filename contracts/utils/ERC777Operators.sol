// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

library ERC777Operators {

    string constant private _ERR_SELF_AUTHORIZE_AS_OPERATOR = "ERC777Operators: authorizing self";

    /// @dev ERC777 operators support self structure
    struct Object {
        address[] defaultOperatorsArray;
        mapping(address => bool) defaultOperators;
        mapping(address => mapping(address => bool)) operators;
        mapping(address => mapping(address => bool)) revokedDefaultOperators;
    }

    function isOperatorFor(Object storage self, address operator, address tokenHolder) internal view returns (bool) {
        return operator == tokenHolder ||
            (
                self.defaultOperators[operator] &&
                !self.revokedDefaultOperators[tokenHolder][operator]
            ) ||
            self.operators[tokenHolder][operator];
    }

    function authorize(Object storage self, address holder, address operator) internal {
        require(holder != operator, _ERR_SELF_AUTHORIZE_AS_OPERATOR);

        if (self.defaultOperators[operator]) {
            delete self.revokedDefaultOperators[holder][operator];
        } else {
            self.operators[holder][operator] = true;
        }
    }

    function revoke(Object storage self, address holder, address operator) internal {
        if (self.defaultOperators[operator]) {
            self.revokedDefaultOperators[holder][operator] = true;
        } else {
            delete self.operators[holder][operator];
        }
    }

    function defaultList(Object storage self) internal view returns (address[] memory) {
        return self.defaultOperatorsArray;
    }

}
