// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

/**
 * @title Constant flow agreement v1 library
 * @author Superfluid
 * @dev for working with the constant flow agreement within solidity
 * @dev the first set of functions are each for callAgreement()
 * @dev the second set of functions are each for use in callAgreementWithContext()
 */
library CFAv1Library {

    struct InitData {
        ISuperfluid host;
        IConstantFlowAgreementV1 cfa;
    }

    /**
     * @dev Create/update/delete flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function flow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
        if ( flowRate == int96(0) ) {
            deleteFlow(cfaLibrary, address(this), receiver, token);
        } else {
            (,int96 existingFlowRate,,) = cfaLibrary.cfa.getFlow(token, address(this), receiver);
            if ( existingFlowRate == int96(0) ) {
                createFlow(cfaLibrary, receiver, token, flowRate);
            } else {
                updateFlow(cfaLibrary, receiver, token, flowRate);
            }
        }
    }

    /**
     * @dev Create/update/delete flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function flow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
        if ( flowRate == int96(0) ) {
            deleteFlow(cfaLibrary, address(this), receiver, token, userData);
        } else {
            (,int96 existingFlowRate,,) = cfaLibrary.cfa.getFlow(token, address(this), receiver);
            if ( existingFlowRate == int96(0) ) {
                createFlow(cfaLibrary, receiver, token, flowRate, userData);
            } else {
                updateFlow(cfaLibrary, receiver, token, flowRate, userData);
            }
        }
    }

    /**
     * @dev Create flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function createFlow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x" // empty user data
        );
    }
    
    /**
     * @dev Create flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function createFlow(
        InitData storage cfaLibrary, 
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

    /**
     * @dev Update flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function updateFlow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x" // empty user data
        );
    }
    

    /**
     * @dev Update flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function updateFlow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

    /**
     * @dev Delete flow without userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param token The token to flow
     */
    function deleteFlow(
        InitData storage cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            "0x" // empty user data
        );
    }
    

    /**
     * @dev Delete flow with userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param userData The user provided data
     */
    function deleteFlow(
        InitData storage cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

    /**
     * @dev Create/update/delete flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function flowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
        if ( flowRate == int96(0) ) {
            return deleteFlowWithCtx(cfaLibrary, ctx, address(this), receiver, token);
        } else {
            (,int96 existingFlowRate,,) = cfaLibrary.cfa.getFlow(token, address(this), receiver);
            if ( existingFlowRate == int96(0) ) {
                return createFlowWithCtx(cfaLibrary, ctx, receiver, token, flowRate);
            } else {
                return updateFlowWithCtx(cfaLibrary, ctx, receiver, token, flowRate);
            }
        }
    }

    /**
     * @dev Create/update/delete flow with context and userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function flowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        if ( flowRate == int96(0) ) {
            return deleteFlowWithCtx(cfaLibrary, ctx, address(this), receiver, token, userData);
        } else {
            (,int96 existingFlowRate,,) = cfaLibrary.cfa.getFlow(token, address(this), receiver);
            if ( existingFlowRate == int96(0) ) {
                return createFlowWithCtx(cfaLibrary, ctx, receiver, token, flowRate, userData);
            } else {
                return updateFlowWithCtx(cfaLibrary, ctx, receiver, token, flowRate, userData);
            }
        }
    }

    /**
     * @dev Create flow with context and userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function createFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x", // empty user data
            ctx
        );
    }

    /**
     * @dev Create flow with context and userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function createFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData,
            ctx
        );
    }

    /**
     * @dev Update flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     */
    function updateFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x", // empty user data
            ctx
        );
    }

    /**
     * @dev Update flow with context and userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param flowRate The desired flowRate
     * @param userData The user provided data
     */
    function updateFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData,
            ctx
        );
    }

    /**
     * @dev Delete flow with context
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param token The token to flow
     */
    function deleteFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            "0x", //empty user data
            ctx
        );
    }

    /**
     * @dev Delete flow with context and userData
     * @param cfaLibrary The cfaLibrary storage variable
     * @param ctx Context bytes (see ISuperfluid.sol for Context struct)
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow
     * @param token The token to flow
     * @param userData The user provided data
     */
    function deleteFlowWithCtx(
        InitData storage cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.cfa,
            abi.encodeWithSelector(
                cfaLibrary.cfa.deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            userData,
            ctx
        );
    }
}
