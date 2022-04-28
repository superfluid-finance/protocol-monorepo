// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { IERC777 } from "@openzeppelin/contracts/token/ERC777/IERC777.sol";
import { IERC1820Registry } from "@openzeppelin/contracts/utils/introspection/IERC1820Registry.sol";
import { IERC777Recipient } from "@openzeppelin/contracts/token/ERC777/IERC777Recipient.sol";

/**
 * @title Token custodian contract
 * @author Superfluid
 * @dev Contract which takes custody of funds which couldn't be sent to the designated recipient
 */
contract TokenCustodian is IERC777Recipient {
    IERC1820Registry constant internal _ERC1820_REGISTRY =
    IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
    mapping(IERC777 => mapping(address => uint256)) public balances;

    constructor() {
        _ERC1820_REGISTRY.setInterfaceImplementer(address(this), keccak256("ERC777TokensRecipient"), address(this));
    }

    event CustodianDeposit(IERC777 indexed token, address recipient, uint256 amount);
    event CustodianWithdrawal(IERC777 indexed token, address recipient, uint256 amount);

    // transfers tokens it has in custody for the given recipient to it
    function flush(IERC777 token, address recipient) public {
        uint256 amount = balances[token][recipient];
        if (amount > 0) {
            balances[token][recipient] = 0;
            // solhint-disable-next-line check-send-result
            token.send(recipient, amount, "0x0");
            emit CustodianWithdrawal(token, recipient, amount);
        }
    }

    // ============ IERC777Recipient ============

    // interface through which ERC777 tokens are deposited to this contract.
    // requires userData to contain an abi-encoded address which designates the recipient for which the deposit is made.
    function tokensReceived(
        address /*operator*/,
        address /*from*/,
        address /*to*/,
        uint256 amount,
        bytes calldata userData,
        bytes calldata /*operatorData*/
    ) override external {
        IERC777 token = IERC777(msg.sender);
        // note: if userData is not set or not decodable as address, this will revert
        address recipient = abi.decode(userData, (address));
        balances[token][recipient] += amount;
        emit CustodianDeposit(token, recipient, amount);
        // note: anybody could call this function and store balance entries. We can just ignore that, does no harm.
    }
}
