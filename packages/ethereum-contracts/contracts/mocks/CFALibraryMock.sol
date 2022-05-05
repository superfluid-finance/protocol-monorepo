// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import {ISuperfluid, ISuperfluidToken, ISuperToken} from "../interfaces/superfluid/ISuperfluid.sol";

import {ISuperApp, ISuperAgreement} from "../interfaces/superfluid/ISuperfluid.sol";

import {ContextDefinitions, SuperAppDefinitions} from "../interfaces/superfluid/ISuperfluid.sol";

import {SuperAppBase} from "../apps/SuperAppBase.sol";

import {IConstantFlowAgreementV1} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import {ERC721} from "@openzeppelin/contracts/token/ERC721/ERC721.sol";

import {CFAv1Library} from "../apps/CFAv1Library.sol";

contract CFALibraryMock {
    using CFAv1Library for CFAv1Library.InitData;

    //initialize cfaV1 variable
    CFAv1Library.InitData public cfaV1;

    constructor(ISuperfluid host) {
        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256(
                            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                        )
                    )
                )
            )
        );
    }

    function createFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.createFlow(receiver, token, flowRate);
    }

    function createFlowWithUserDataTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.createFlow(receiver, token, flowRate, userData);
    }

    function updateFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.updateFlow(receiver, token, flowRate);
    }

    function updateFlowWithUserDataTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.updateFlow(receiver, token, flowRate, userData);
    }

    function deleteFlowTest(ISuperfluidToken token, address receiver) public {
        cfaV1.deleteFlow(address(this), receiver, token);
    }

    function deleteFlowWithUserDataTest(
        ISuperfluidToken token,
        address receiver,
        bytes memory userData
    ) public {
        cfaV1.deleteFlow(address(this), receiver, token, userData);
    }

    function createFlowWithCtxTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.createFlowWithCtx(ctx, receiver, token, flowRate);
    }

    function createFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.createFlowWithCtx(ctx, receiver, token, flowRate, userData);
    }

    function updateFlowWithCtxTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.updateFlowWithCtx(ctx, receiver, token, flowRate);
    }

    function updateFlowWithCtxUserDataTest(
        bytes memory ctx,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate,
        bytes memory userData
    ) public {
        cfaV1.updateFlowWithCtx(ctx, receiver, token, flowRate, userData);
    }

    function deleteFlowWithCtxTest(
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlowWithCtx(ctx, sender, receiver, token);
    }

    function deleteFlowWithCtxUserDataTest(
        bytes memory ctx,
        address sender,
        address receiver,
        ISuperfluidToken token,
        bytes memory userData
    ) public {
        cfaV1.deleteFlowWithCtx(ctx, sender, receiver, token, userData);
    }

    function createFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.createFlowByOperator(sender, receiver, token, flowRate);
    }

    function updateFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.updateFlowByOperator(sender, receiver, token, flowRate);
    }

    function deleteFlowByOperator(
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlowByOperator(sender, receiver, token);
    }

    function updateFlowOperatorPermissionsTest(
        address flowOperator,
        ISuperfluidToken token,
        uint8 permissions,
        int96 flowRateAllowance
    ) public {
        cfaV1.updateFlowOperatorPermissions(flowOperator, token, permissions, flowRateAllowance);
    }

    function authorizeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.authorizeFlowOperatorWithFullControl(flowOperator, token);
    }

    function revokeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.revokeFlowOperatorWithFullControl(flowOperator, token);
    }
}

contract RedirectAllMock is SuperAppBase {
    using CFAv1Library for CFAv1Library.InitData;

    //initialize cfaV1 variable
    CFAv1Library.InitData public cfaV1;

    ISuperfluid private _host; // host
    IConstantFlowAgreementV1 private _cfa; // the stored constant flow agreement class address
    ISuperToken private _acceptedToken; // accepted token
    address private _receiver;

    constructor(
        ISuperfluid host,
        ISuperToken acceptedToken,
        address receiver
    ) {
        assert(address(host) != address(0));
        assert(address(acceptedToken) != address(0));
        assert(address(receiver) != address(0));
        //assert(!_host.isApp(ISuperApp(receiver)));

        _host = host;
        _cfa = IConstantFlowAgreementV1(
            address(
                host.getAgreementClass(
                    keccak256(
                        "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                    )
                )
            )
        );
        _acceptedToken = acceptedToken;
        _receiver = receiver;

        cfaV1 = CFAv1Library.InitData(_host, _cfa);

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
            // change from 'before agreement stuff to after agreement
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    /**************************************************************************
     * Redirect Logic
     *************************************************************************/

    function currentReceiver()
        external
        view
        returns (
            uint256 startTime,
            address receiver,
            int96 flowRate
        )
    {
        if (_receiver != address(0)) {
            (startTime, flowRate, , ) = _cfa.getFlow(
                _acceptedToken,
                address(this),
                _receiver
            );
            receiver = _receiver;
        }
    }

    event ReceiverChanged(address receiver);

    /// @dev If a new stream is opened, or an existing one is opened
    function _updateOutflow(bytes calldata ctx)
        private
        returns (bytes memory newCtx)
    {
        newCtx = ctx;
        // @dev This will give me the new flowRate, as it is called in after callbacks
        int96 netFlowRate = _cfa.getNetFlow(_acceptedToken, address(this));
        (, int96 outFlowRate, , ) = _cfa.getFlow(
            _acceptedToken,
            address(this),
            _receiver
        );
        int96 inFlowRate = netFlowRate + outFlowRate;
        if (inFlowRate < 0) {
            inFlowRate = inFlowRate * -1; // Fixes issue when inFlowRate is negative
        }
        // @dev If inFlowRate === 0, then delete existing flow.
        if (inFlowRate == int96(0)) {
            //if previous flow rate (i.e. current outflow rate from contract to receiver)
            //is > 10 super tokens/month, pass w user data
            if (outFlowRate > 3858024691358) {
                newCtx = cfaV1.deleteFlowWithCtx(
                    newCtx,
                    address(this),
                    _receiver,
                    _acceptedToken,
                    abi.encodePacked(
                        "adding user data because flow greater than 10"
                    )
                );
            } else {
                //otherwise, don't
                newCtx = cfaV1.deleteFlowWithCtx(
                    newCtx,
                    address(this),
                    _receiver,
                    _acceptedToken
                );
            }
        }
        //if flow exists, update the flow
        else if (outFlowRate != int96(0)) {
            //if new inflow rate flow rate (i.e. new inflow to contract) is > 10 super tokens/month, pass user data
            if (inFlowRate > 3858024691358) {
                newCtx = cfaV1.updateFlowWithCtx(
                    newCtx,
                    _receiver,
                    _acceptedToken,
                    inFlowRate,
                    abi.encodePacked(
                        "adding user data because inflow greater than 10"
                    )
                );
            } else {
                //otherwise, don't
                newCtx = cfaV1.updateFlowWithCtx(
                    newCtx,
                    _receiver,
                    _acceptedToken,
                    inFlowRate
                );
            }
        } else {
            // @dev If there is no existing outflow, then create new flow to equal inflow
            //if new inflow rate flow rate (i.e. new inflow to contract) is > 10 super tokens/month, pass user data
            if (inFlowRate > 3858024691358) {
                newCtx = cfaV1.createFlowWithCtx(
                    newCtx,
                    _receiver,
                    _acceptedToken,
                    inFlowRate,
                    abi.encodePacked(
                        "adding user data because inflow greater than 10"
                    )
                );
            }
            //otherwise, don't
            else {
                newCtx = cfaV1.createFlowWithCtx(
                    newCtx,
                    _receiver,
                    _acceptedToken,
                    inFlowRate
                );
            }
        }
    }

    // @dev Change the Receiver of the total flow
    function _changeReceiver(address newReceiver) internal {
        require(newReceiver != address(0), "New receiver is zero address");
        // @dev because our app is registered as final, we can't take downstream apps
        require(
            !_host.isApp(ISuperApp(newReceiver)),
            "New receiver can not be a superApp"
        );
        if (newReceiver == _receiver) return;
        // @dev delete flow to old receiver
        cfaV1.deleteFlow(address(this), _receiver, _acceptedToken);
        // @dev create flow to new receiver
        cfaV1.createFlow(
            newReceiver,
            _acceptedToken,
            _cfa.getNetFlow(_acceptedToken, address(this))
        );

        // @dev set global receiver to new receiver
        _receiver = newReceiver;

        emit ReceiverChanged(_receiver);
    }

    //public variables which we'll set userData values to
    ISuperfluid.Context public uData;
    string public userData;

    /**************************************************************************
     * SuperApp callbacks
     *************************************************************************/

    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, // _agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, // _cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyExpected(superToken, agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        // decode Context - store full context as uData variable for easy visualization purposes
        ISuperfluid.Context memory decompiledContext = _host.decodeCtx(ctx);
        uData = decompiledContext;

        //set userData variable to decoded value
        //for now, this value is hardcoded as a string
        //- this will be made clear in flow creation scripts within the tutorial
        //this string will serve as a message when a flow is created with recipient = TCF
        //it will be displayed on a front end for assistance in userData explanation
        userData = abi.decode(decompiledContext.userData, (string));

        return _updateOutflow(ctx);
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, //_agreementId,
        bytes calldata, /*_agreementData*/
        bytes calldata, //_cbdata,
        bytes calldata ctx
    )
        external
        override
        onlyExpected(superToken, agreementClass)
        onlyHost
        returns (bytes memory newCtx)
    {
        ISuperfluid.Context memory decompiledContext = _host.decodeCtx(ctx);
        uData = decompiledContext;

        userData = abi.decode(decompiledContext.userData, (string));

        return _updateOutflow(ctx);
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32, //_agreementId,
        bytes calldata, // _agreementData,
        bytes calldata, //_cbdata,
        bytes calldata ctx
    ) external override onlyHost returns (bytes memory newCtx) {
        // According to the app basic law, we should never revert in a termination callback
        if (!_isSameToken(superToken) || !_isCFAv1(agreementClass)) return ctx;

        ISuperfluid.Context memory decompiledContext = _host.decodeCtx(ctx);
        uData = decompiledContext;

        userData = abi.decode(decompiledContext.userData, (string));

        return _updateOutflow(ctx);
    }

    function _isSameToken(ISuperToken superToken) private view returns (bool) {
        return address(superToken) == address(_acceptedToken);
    }

    function _isCFAv1(address agreementClass) private view returns (bool) {
        return
            ISuperAgreement(agreementClass).agreementType() ==
            keccak256(
                "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
            );
    }

    modifier onlyHost() {
        require(
            msg.sender == address(_host),
            "RedirectAll: support only one host"
        );
        _;
    }

    modifier onlyExpected(ISuperToken superToken, address agreementClass) {
        require(_isSameToken(superToken), "RedirectAll: not accepted token");
        require(_isCFAv1(agreementClass), "RedirectAll: only CFAv1 supported");
        _;
    }
}

contract TradeableCashflowMock is ERC721, RedirectAllMock {
    constructor(
        address owner,
        string memory name,
        string memory symbol,
        ISuperfluid host,
        ISuperToken acceptedToken
    ) ERC721(name, symbol) RedirectAllMock(host, acceptedToken, owner) {
        _mint(owner, 1);
    }

    //now I will insert a nice little hook in the _transfer, including the RedirectAll function I need
    function _beforeTokenTransfer(
        address, /*from*/
        address to,
        uint256 /*tokenId*/
    ) internal override {
        _changeReceiver(to);
    }
}

contract SuperAppFlowOperatorMock is SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData internal cfaV1;

    // default values for smoke tests
    uint8 internal immutable permissions = 7;
    int96 internal immutable flowRate = 100000000;
    address internal immutable sender;
    address internal immutable receiver;
    address internal immutable flowOperator;

    // for selectively testing functions in the same callback
    enum FunctionIndex {
        CREATE_FLOW_BY_OPERATOR,
        UPDATE_FLOW_BY_OPERATOR,
        DELETE_FLOW_BY_OPERATOR,
        UPDATE_FLOW_OPERATOR_PERMISSIONS,
        AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL,
        REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL
    }

    constructor(
        ISuperfluid host,
        address defaultSender,
        address defaultReceiver,
        address defaultFlowOperator
    ) {
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256(
                            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                        )
                    )
                )
            )
        );
        sender = defaultSender;
        receiver = defaultReceiver;
        flowOperator = defaultFlowOperator;

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            // SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;

        host.registerApp(configWord);
    }

    // literally ONLY for the revokeFlowOperatorWithFullControlWithCtx test.
    function authorizeFlowOperatorWithFullControl(ISuperToken token) external {
        cfaV1.authorizeFlowOperatorWithFullControl(flowOperator, token);
    }

    function afterAgreementCreated(
        ISuperToken token,
        address,
        bytes32,
        bytes calldata,
        bytes calldata,
        bytes calldata ctx
    ) external override returns (bytes memory) {
        bytes memory userData = cfaV1.host.decodeCtx(ctx).userData;
        (uint8 functionIndex) = abi.decode(userData, (uint8));

        if (functionIndex == uint8(FunctionIndex.CREATE_FLOW_BY_OPERATOR))
            return cfaV1.createFlowByOperatorWithCtx(ctx, sender, receiver, token, flowRate);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_BY_OPERATOR))
            return cfaV1.updateFlowByOperatorWithCtx(ctx, sender, receiver, token, flowRate);
        else if (functionIndex == uint8(FunctionIndex.DELETE_FLOW_BY_OPERATOR))
            return cfaV1.deleteFlowByOperatorWithCtx(ctx, sender, receiver, token);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_OPERATOR_PERMISSIONS))
            return cfaV1.updateFlowOperatorPermissionsWithCtx(
                ctx,
                flowOperator,
                token,
                permissions,
                flowRate
            );
        else if (functionIndex == uint8(FunctionIndex.AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.authorizeFlowOperatorWithFullControlWithCtx(ctx, flowOperator, token);
        else if (functionIndex == uint8(FunctionIndex.REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.revokeFlowOperatorWithFullControlWithCtx(ctx, flowOperator, token);
        else revert("invalid function index");
    }
}
