// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

import { ISuperfluid, ISuperToken, SuperAppBase, ISuperApp, SuperAppDefinitions } from "../apps/SuperAppBase.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperfluid.sol";

contract StreamRedirector is SuperAppBase {
    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData public cfaV1;
    ISuperfluid public host;
    bytes32 public constant CFA_ID =
        keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    address public receiver; // flow is redirected to hardcoded receiver address
    ISuperToken public token;

    constructor(
        ISuperfluid _host,
        ISuperToken _token,
        address _receiver,
        uint256 _appLevel
    ) {
        assert(address(_token) != address(0));
        assert(address(_receiver) != address(0));
        assert(address(_host) != address(0));

        host = _host;

        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
            host,
            //here, we are deriving the address of the CFA using the host contract
            IConstantFlowAgreementV1(address(host.getAgreementClass(CFA_ID)))
        );

        uint256 configWord = _appLevel |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        token = _token;
        receiver = _receiver;

        host.registerAppWithKey(configWord, "");
    }

    /**
     * Custom Errors
     */
    error OnlyHost();
    error OnlyCFA();

    /**
     * Creates a stream from an _originAccount who has granted this SuperApp ACL permissions
     * to this SuperApp.
     * @param _originAccount ACL permissions granter
     * @param _flowRate desired flow rate
     */
    function startStreamToSelf(address _originAccount, int96 _flowRate) public {
        cfaV1.createFlowByOperator(
            _originAccount,
            address(this),
            token,
            _flowRate
        );
    }

    /**
     * A callback which occurs after CF agreement creation which redirects a stream.
     */
    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // agreementId,
        bytes calldata, // agreementData,
        bytes calldata, // cbdata,
        bytes calldata ctx
    ) external override onlyCFA(agreementClass) returns (bytes memory newCtx) {
        newCtx = ctx;
        int96 netFlowRate = cfaV1.cfa.getNetFlow(superToken, address(this));
        newCtx = cfaV1.createFlowWithCtx(
            newCtx,
            receiver,
            superToken,
            netFlowRate
        );
    }

    /**
     * Alleviates the composite app rule by allowing this app to call
     * the target app.
     * @param _targetApp address of the target super app, should be initialized with final_level
     */
    function allowCompositeApp(address _targetApp) public {
        host.allowCompositeApp(ISuperApp(_targetApp));
    }

    function _isCFAv1(address agreementClass) private view returns (bool) {
        return ISuperAgreement(agreementClass).agreementType() == CFA_ID;
    }

    /**
     * Modifiers
     */
    modifier onlyHost() {
        if (msg.sender != address(host)) revert OnlyHost();
        _;
    }

    modifier onlyCFA(address agreementClass) {
        if (!_isCFAv1(agreementClass)) revert OnlyCFA();
        _;
    }
}
