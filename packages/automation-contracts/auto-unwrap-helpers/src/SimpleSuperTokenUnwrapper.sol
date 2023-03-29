// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import { Clones } from "@openzeppelin/contracts/proxy/Clones.sol";
import { Initializable } from "@openzeppelin/contracts/proxy/utils/Initializable.sol";
import {
    IERC20, ISuperToken
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperToken.sol";

contract SimpleSuperTokenUnwrapper is Initializable {
    address payable public payer;
    address public designatedToken;
    address public payee;

    function initialize(address payable payer_,
                        address designatedToken_,
                        address payee_) external
        initializer()
    {
        payer = payer_;
        designatedToken = designatedToken_;
        payee = payee_;

        require(ISuperToken(designatedToken).getHost() != address(0), "You imposter!");
    }

    function refundERC20(address token) external returns (bool) {
        // Etherscan may not show custom error nicely yet
        require(token != designatedToken, "Designated token goes to payee");
        // NOTE: let's not assume the erc20 token has correct transfer implementation that returns a true
        IERC20(token).transfer(payer, IERC20(token).balanceOf(address(this)));
        return true;
    }

    function releaseAll() external {
        // release designated super tokens to the payee
        ISuperToken t = ISuperToken(designatedToken);
        t.downgradeTo(payee, t.balanceOf(address(this)));
    }
}

contract StreamingInvoiceTemplates {
    address public immutable SIMPLE_SUPER_TOKEN_UNWRAPPER_LOGIC;

    constructor () {
        SIMPLE_SUPER_TOKEN_UNWRAPPER_LOGIC = address(new SimpleSuperTokenUnwrapper());
    }

    function createSimpleSuperTokenUnwrapper(address payer,
                                             address designatedToken,
                                             address payee) external
        returns (SimpleSuperTokenUnwrapper a)
    {
        a = SimpleSuperTokenUnwrapper(Clones.clone(SIMPLE_SUPER_TOKEN_UNWRAPPER_LOGIC));
        a.initialize(payable(payer), designatedToken, payee);
    }
}
