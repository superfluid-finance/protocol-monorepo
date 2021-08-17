pragma solidity >= 0.7.0;


import { ISuperfluid, ISuperAgreement } from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

/**
 * @title Superfluid's batch liquidations
 *
 * @author Superfluid
 */

contract BatchLiquidator is Ownable {

    function deleteFlows(
        address host,
        address cfa,
        address superToken,
        address[] calldata senders, address[] calldata receivers
    ) external {
        for (uint i = 0; i < senders.length; ++i) {
            bool success;
            // solhint-disable-next-line avoid-low-level-calls
            (success, ) = address(host).call(
                abi.encodeWithSelector(
                    ISuperfluid.callAgreement.selector,
                    cfa,
                    abi.encodeWithSelector(
                        IConstantFlowAgreementV1.deleteFlow.selector,
                        superToken,
                        senders[i],
                        receivers[i],
                        new bytes(0)
                    ),
                    new bytes(0)
                )
            );
        }
        _transferFrom(
            superToken,
            msg.sender,
            ERC20(superToken).balanceOf(address(this))
        );
    }

    function transfer(address token) external onlyOwner {
        assert(
            _transferFrom(
                token,
                owner(),
                ERC20(token).balanceOf(address(this))
            )
        );
    }

    function transferTo(address token, address to, uint256 amount) external onlyOwner {
        assert(_transferFrom(token, to, amount));
    }

    function _transferFrom(address token, address to, uint256 amount) internal returns(bool) {
        return ERC20(token).transferFrom(address(this), to, amount);
    }
}

