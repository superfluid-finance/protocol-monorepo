//SPDX-License-Identifier: AGPLv3
pragma solidity ^0.7.0;
pragma experimental ABIEncoderV2;

import {
    ISuperfluid,
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluid.sol";

import {
    IConstantFlowAgreementV1
} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

library CFALibraryV1 {

    struct InitData {
        ISuperfluid host;
    }

    //@dev for working with the constant flow agreement within solidity
    //the first set of functions are each for callAgreement()
    //the second set of functions are each for use in callAgreementWithContext()

     //create flow without userData
    function createFlow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x" //empty user data
        );
    }
    

    //create flow with userData
    function createFlow(
        InitData storage cfaLibrary, 
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

    //update flow without userData
    function updateFlow(
        InitData storage cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x" //empty user data
        );
    }
    

    //update flow with userData
    function updateFlow(
        InitData memory cfaLibrary,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

    //delete flow
    function deleteFlow(
        InitData memory cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            "0x" //empty user data
        );
    }
    

    //delete flow with userData 
    function deleteFlow(
        InitData memory cfaLibrary,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal {
        cfaLibrary.host.callAgreement(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            userData
        );
    }

  //create flow with ctx 
    function createFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x", //empty user data
            ctx
        );
    }

    //create flow with ctx and userData
    function createFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).createFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData,
            ctx
        );
    }

  //update flow with ctx but without userData 
    function updateFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            "0x", //empty user data
            ctx
        );
    }

    //update flow with ctx and userData
    function updateFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).updateFlow.selector,
                token,
                receiver,
                flowRate,
                new bytes(0) // placeholder
            ),
            userData,
            ctx
        );
    }

  //delete flow with ctx 
    function deleteFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).deleteFlow.selector,
                token,
                sender,
                receiver,
                new bytes(0) // placeholder
            ),
            "0x", //empty user data
            ctx
        );
    }

    //delete flow with ctx and userData 
    function deleteFlowWithCtx(
        InitData memory cfaLibrary,
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) internal returns (bytes memory newCtx) {
        (newCtx, ) = cfaLibrary.host.callAgreementWithContext(
            cfaLibrary.host.getAgreementClass(keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")),
            abi.encodeWithSelector(
                IConstantFlowAgreementV1(
                    address(cfaLibrary.host.getAgreementClass(
                        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
                        )
                    )
                ).deleteFlow.selector,
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