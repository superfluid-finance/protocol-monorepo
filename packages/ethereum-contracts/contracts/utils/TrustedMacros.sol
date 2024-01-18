// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { ISuperfluid, ISuperToken, BatchOperation } from "../interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";
import { ForwarderBase } from "../utils/ForwarderBase.sol";


// User-defined macro
interface IUserDefinedMacro {
    function executeMacro(ISuperfluid host, bytes memory params) external view
        returns (ISuperfluid.Operation[] memory operations);
    function hash712Params(bytes memory params) external view returns (bytes32 hash);
}

library EIP712Helper {
    string constant DOMAIN_TYPE =
        "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract,bytes32 salt)";
    bytes32 constant DOMAIN_TYPEHASH = keccak256(abi.encodePacked(DOMAIN_TYPE));

    function createDomainSeparator(string memory name, string memory version,
                                   address verifyingContract, bytes32 salt) internal view returns (bytes32)
    {
        return keccak256(abi.encode(DOMAIN_TYPEHASH,
                                    keccak256(bytes(name)),
                                    keccak256(bytes(version)),
                                    block.chainid,
                                    verifyingContract,
                                    salt));
    }
}

// Note:
//
// * A Superfluid trusted forwarder
// * Extensible with user-defined macros
contract TrustedMacros is ForwarderBase {
    constructor(ISuperfluid host) ForwarderBase(_host) {}

    // Vanilla variant:

    function simulateMacro(IUserDefinedMacro m, bytes memory params) public view
        returns (ISuperfluid.Operation[] memory operations)
    {
        operations = m.executeMacro(_host, params);
    }

    function runMacro(IUserDefinedMacro m, bytes memory params) external returns (bool)
    {
        ISuperfluid.Operation[] memory operations = simulateMacro(m, params);
        return _forwardBatchCall(operations);
    }

    // EIP-712 variant:
    string public constant MACRO_REQUEST_TYPE
        = "MacroRequest(string lang, string requestMessage, params)";

    // These manifests will give additional built-in security support when using EIP-712.
    struct Macro712Manifest {
        // EIP712 domain components
        string name; // e.g. "tokenBatchStreamer"
        string version; // e.g. "1.0.0"
        bytes32 salt;
        // Trusted macro components
        string origReqMsg; // e.g "Batch creation of money streams of one token"
        mapping (string => string) transReqMsgs;
        string paramsTypeName; // e.g. "TokenBatchStreamerParams"
        string paramsTypeArgs; // e.g. "(address token, address[] recipients)"
    }

    mapping (string => address) public macro712NameOwners;
    mapping (IUserDefinedMacro => Macro712Manifest) public macro712Manifests;

    /* function transferMacro712NameOwner() external { */
    /* } */

    /* function registerMacro712Manifest(Macro712Manifest manifest) external { */
    /* } */

    function validateParams712Hash(address signer, bytes32 hash, uint8 sigV, bytes32 sigR, bytes32 sigS) pure
        public returns (bool)
    {
        return signer == ecrecover(hash, sigV, sigR, sigS);
    }

    function simulateMacro712(IUserDefinedMacro m, bytes memory params,
                              string memory lang, uint8 sigV, bytes32 sigR, bytes32 sigS
                             ) public view
        returns (ISuperfluid.Operation[] memory operations)
    {
        bytes32 params712Hash;
        {
            Macro712Manifest storage manifest = macro712Manifests[m];
            bytes32 macroReqHash;
            {
                string memory macroReqType = string.concat("MacroRequest(string lang,string requestMessage,",
                                                           manifest.paramsTypeName, " params)");
                macroReqHash = keccak256(abi.encodePacked(macroReqType));
            }
            bytes32 payloadSeparator;
            {
                string memory requestMessage = manifest.transReqMsgs[lang];
                if (bytes(requestMessage).length == 0) {
                    requestMessage = manifest.origReqMsg;
                }
                payloadSeparator = keccak256(abi.encode(macroReqHash,
                                                        lang,
                                                        requestMessage,
                                                        m.hash712Params(params)));
            }
            bytes32 domainSeparator =
                EIP712Helper.createDomainSeparator(manifest.name, manifest.version, address(this), manifest.salt);
            params712Hash = keccak256(abi.encodePacked("\x19\x01", domainSeparator, payloadSeparator));
        }
        require(validateParams712Hash(msg.sender, params712Hash, sigV, sigR, sigS), "bad 712 hash");
        operations = m.executeMacro(_host, params);
    }

    function runMacro712(IUserDefinedMacro m, bytes memory params,
                         string memory lang, uint8 sigV, bytes32 sigR, bytes32 sigS
                        ) external returns (bool)
    {
        ISuperfluid.Operation[] memory operations = simulateMacro712(m, params, lang, sigV, sigR, sigS);
        return _forwardBatchCall(operations);
    }
}

contract TokenBatchStreamerMacro is IUserDefinedMacro {
    function executeMacro(ISuperfluid host, bytes memory params) external view
        returns (ISuperfluid.Operation[] memory operations)
    {
        IConstantFlowAgreementV1 cfa = IConstantFlowAgreementV1(address(host.getAgreementClass(
            keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1")
        )));
        (ISuperToken token, int96 flowRate, address[] memory recipients) =
            abi.decode(params, (ISuperToken, int96, address[]));
        operations = new ISuperfluid.Operation[](recipients.length);
        // Build batch call operations here
        for (uint i = 0; i < recipients.length; ++i) {
            bytes memory callData = abi.encodeCall(cfa.createFlow,
                                                   (token,
                                                    recipients[i],
                                                    flowRate,
                                                    new bytes(0) // placeholder
                                                   ));
            operations[i] = ISuperfluid.Operation({
                operationType : BatchOperation.OPERATION_TYPE_SUPERFLUID_CALL_AGREEMENT, // type
                target: address(cfa),
                data: abi.encode(callData, new bytes(0))
                });
        }
    }

    function hash712Params(bytes memory params) external view returns (bytes32 hash) {
        (ISuperToken token, int96 flowRate, address[] memory recipients) =
            abi.decode(params, (ISuperToken, int96, address[]));
    }
}
