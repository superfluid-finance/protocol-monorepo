// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IUserDefinedMacro } from "../interfaces/utils/IUserDefinedMacro.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { ForwarderBase } from "../utils/ForwarderBase.sol";

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

/**
 * @dev This is a trusted forwarder with high degree of extensibility through permission-less and user-defined "macro
 * contracts". This is an EIP-712 version.
 */
contract TrustedMacrosVanilla is ForwarderBase {
    constructor(ISuperfluid host) ForwarderBase(host) {}

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

    function simulateMacro(IUserDefinedMacro m, bytes memory params,
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
                                                        requestMessage
                                                        //m.hash712Params(params) FIXME
                                                       ));
            }
            bytes32 domainSeparator =
                EIP712Helper.createDomainSeparator(manifest.name, manifest.version, address(this), manifest.salt);
            params712Hash = keccak256(abi.encodePacked("\x19\x01", domainSeparator, payloadSeparator));
        }
        require(validateParams712Hash(msg.sender, params712Hash, sigV, sigR, sigS), "bad 712 hash");
        operations = m.executeMacro(_host, params);
    }

    function runMacro(IUserDefinedMacro m, bytes memory params,
                      string memory lang, uint8 sigV, bytes32 sigR, bytes32 sigS
                      ) external returns (bool)
    {
        ISuperfluid.Operation[] memory operations = simulateMacro(m, params, lang, sigV, sigR, sigS);
        return _forwardBatchCall(operations);
    }
}
