// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "../apps/SuperAppBase.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperfluid.sol";

contract StreamRedirector is SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;
    bytes32 constant public CFA_ID = keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    address public receiver; // flow is redirected to hardcoded receiver address
    ISuperToken public token;

    constructor(ISuperfluid host, ISuperToken _token, address _receiver) {
        assert(address(_token) != address(0));
        assert(address(_receiver) != address(0));
        assert(address(host) != address(0));

        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
            host,
            //here, we are deriving the address of the CFA using the host contract
            IConstantFlowAgreementV1(
                address(host.getAgreementClass(CFA_ID))
            )
        );

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        token = _token;
        receiver = _receiver;

        host.registerAppWithKey(configWord, "");
    }

    function startStreamToSelf(address _originAccount, int96 _flowRate) public {
        cfaV1.createFlowByOperator(_originAccount, address(this), token, _flowRate);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // agreementId,
        bytes calldata, // agreementData,
        bytes calldata, // cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyCFA(agreementClass)
        returns(bytes memory newCtx)
    {
        newCtx = ctx;
        int96 netFlowRate = cfaV1.cfa.getNetFlow(superToken, address(this));
        newCtx = cfaV1.createFlowWithCtx(newCtx, receiver, superToken, netFlowRate);
    }


    function _isCFAv1(address agreementClass) private view returns (bool) {
        return ISuperAgreement(agreementClass).agreementType() == CFA_ID;
    }

    modifier onlyHost() {
        require(
            msg.sender == address(cfaV1.host),
            "Only host can call callback"
        );
        _;
    }

    modifier onlyCFA(address agreementClass) {
        require(_isCFAv1(agreementClass), "Only CFAv1 supported");
        _;
    }
}