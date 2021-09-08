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

interface ICLOWNSv1 {
    /// @dev get the address of the current CLO for the given token.
    function getCurrentCLO(ISuperToken token) external view returns(address clo);

    /**
     * @dev get the remaining bond of the current CLO for the given token.
     * NOTES:
     * - the bond changes dynamically and can both grow or shrink between blocks
     * - also returns the CLO address in order to allow for atomic querying (important in a continuous auction)
     */
    function getCurrentCLOBond(ISuperToken token) external view returns(address clo, uint256 remainingBond);

    /**
     * @dev New CLO event
     * @param token The Super token the new CLO bid for
     * @param clo The address of the new CLO
     * @param bond the size (amount) of the bond placed by the CLO
     * @param exitRate the flowrate at which the bond and accrued rewards will be streamed to the CLO
     */
    event NewCLO(ISuperToken indexed token, address clo, uint256 bond, int96 exitRate);
}

// TODO: document contract
// TODO: add method using transferFrom instead of send()
// TODO: check if it works if the bidder is the current CLO
contract CLOWNS is ICLOWNSv1, IERC777Recipient {
    // lightweight struct packing an address and a bool (reentrancy guard) into 1 word
    struct LockableCLO {
        address addr;
        bool lock;
    }
    mapping(ISuperToken => LockableCLO) internal _currentCLOs;
    ISuperfluid internal immutable _host;
    IConstantFlowAgreementV1 internal immutable _cfa;
    uint256 public immutable minBondDuration; // TODO: should maybe be a governance parameter
    IERC1820Registry constant internal _ERC1820_REGISTRY =
        IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);

    constructor(ISuperfluid host_, uint256 minBondDuration_) {
        _host = ISuperfluid(host_);
        minBondDuration = minBondDuration_;
        address cfaAddr = address(
            host_.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1"))
        );
        _cfa = IConstantFlowAgreementV1(cfaAddr);
        _ERC1820_REGISTRY.setInterfaceImplementer(address(this), keccak256("ERC777TokensRecipient"), address(this));
        _ERC1820_REGISTRY.setInterfaceImplementer(address(this), keccak256("CLOWNSv1"), address(this));
    }

    function getCurrentCLO(ISuperToken token) external view override returns(address clo) {
        return _currentCLOs[token].addr;
    }

    function getCurrentCLOBond(ISuperToken token) external view override returns(address clo, uint256 remainingBond) {
        return (_currentCLOs[token].addr, token.balanceOf(address(this)));
    }

    function _becomeCLO(ISuperToken token, address newCLO, uint256 amount, int96 exitRate) internal {
        require(!_currentCLOs[token].lock, "CLOWNS: reentrancy not allowed");
        require(exitRate >= 0, "CLOWNS: negative exitRate not allowed");
        require(uint256(exitRate) * minBondDuration <= amount, "CLOWNS: below minimum bond duration");
        require(amount > token.balanceOf(address(this))-amount, "CLOWNS: bid too low");

        address currentCLOAddr = _currentCLOs[token].addr;
        _currentCLOs[token].lock = true;

        // close stream to current CLO if exists
        (, int96 curFlowRate,,) = _cfa.getFlow(token, address(this), currentCLOAddr);
        if (curFlowRate > 0) {
            try _host.callAgreement(
                _cfa,
                abi.encodeWithSelector(
                    _cfa.deleteFlow.selector,
                    token,
                    address(this),
                    currentCLOAddr,
                    new bytes(0)
                ),
                "0x"
            // solhint-disable-next-line no-empty-blocks
            ) {} catch {}
        }

        // send remaining funds to current CLO
        // send() allows the CLO to automatically re-bid
        // If the current CLO causes the send() to fail, we're sorry for them.
        // solhint-disable-next-line check-send-result
        try token.send(currentCLOAddr, token.balanceOf(address(this))-amount, "0x")
        // solhint-disable-next-line no-empty-blocks
        {} catch {}

        // set new CLO
        // solhint-disable-next-line reentrancy
        _currentCLOs[token].addr = newCLO;

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

        // solhint-disable-next-line reentrancy
        _currentCLOs[token].lock = false;
        emit NewCLO(token, newCLO, amount, exitRate);
    }

    // ============ IERC777Recipient ============

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
