// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperToken,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import { IERC1820Registry } from "@openzeppelin/contracts/introspection/IERC1820Registry.sol";
import { IERC777Recipient } from "@openzeppelin/contracts/token/ERC777/IERC777Recipient.sol";

// TODO: document contract
// TODO: add method using transferFrom instead of send()
// TODO: check if it works if the bidder is the current CLO
contract CLOWNS is IERC777Recipient {
    mapping(ISuperToken => address) public currentCLOs;
    ISuperfluid internal immutable _host;
    IConstantFlowAgreementV1 internal immutable _cfa;
    uint256 public immutable minBondDuration; // TODO: should maybe be a governance parameter
    IERC1820Registry constant internal _ERC1820_REGISTRY =
        IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);

    event NewCLO(address clo, uint256 bond, int96 exitRate);

    constructor(ISuperfluid host_, uint256 minBondDuration_) {
        _host = ISuperfluid(host_);
        minBondDuration = minBondDuration_;
        address cfaAddr = address(
            host_.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1"))
        );
        _cfa = IConstantFlowAgreementV1(cfaAddr);
        _ERC1820_REGISTRY.setInterfaceImplementer(address(this), keccak256("ERC777TokensRecipient"), address(this));
    }

    function _becomeCLO(ISuperToken token, address newCLO, uint256 amount, int96 exitRate) internal {
        address currentCLO = currentCLOs[token];

        require(uint256(exitRate) * minBondDuration <= amount, "CLOWNS: below minimum bond duration");
        require(amount > token.balanceOf(address(this))-amount, "CLOWNS: bid too low");

        // close stream to current CLO if exists
        (, int96 curFlowRate,,) = _cfa.getFlow(token, address(this), currentCLO);
        if (curFlowRate > 0) {
            try _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                    _cfa.deleteFlow.selector,
                    token,
                    address(this),
                    currentCLO,
                    new bytes(0)
                ),
                "0x"
            // solhint-disable-next-line no-empty-blocks
            ) {} catch {}
        }

        // send remaining funds to current CLO
        // send() allows the CLO to automatically re-bid
        // TODO: reentrancy prevention
        // If the current CLO causes the send() to fail, we're sorry for them.
        // solhint-disable-next-line check-send-result
        try token.send(currentCLO, token.balanceOf(address(this))-amount, "0x")
        // solhint-disable-next-line no-empty-blocks
        {} catch {}

        // set new CLO
        // TODO: check if this is really safe - probably only for single-level reentrancy
        // solhint-disable-next-line reentrancy
        currentCLOs[token] = newCLO;

        // open stream to new CLO if exitRate > 0
        if (exitRate > 0) {
            _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                    _cfa.createFlow.selector,
                    token,
                    newCLO,
                    exitRate,
                    new bytes(0)
                ),
                "0x"
            );
        }

        emit NewCLO(newCLO, amount, exitRate);
    }

    function tokensReceived(
        address /*operator*/,
        address from,
        address /*to*/,
        uint256 amount,
        bytes calldata userData,
        bytes calldata /*operatorData*/
    ) override external {
        // if it's not a SuperToken, something will revert along the way
        ISuperToken token = ISuperToken(msg.sender);

        // will revert if not a valid int96
        int96 exitRate = abi.decode(userData, (int96));
        _becomeCLO(token, from, amount, exitRate);
    }
}
