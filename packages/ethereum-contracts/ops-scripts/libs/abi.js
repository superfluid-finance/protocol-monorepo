if (typeof module === "undefined") module = {};
// eslint-disable-next-line no-unused-vars
let Superfluid_ABI;
Superfluid_ABI = module.exports = {
    Ownable: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "previousOwner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "newOwner",
        "type": "address"
      }
    ],
    "name": "OwnershipTransferred",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "owner",
    "outputs": [
      {
        "internalType": "address",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "renounceOwnership",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "newOwner",
        "type": "address"
      }
    ],
    "name": "transferOwnership",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    AccessControl: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "bytes32",
        "name": "previousAdminRole",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "bytes32",
        "name": "newAdminRole",
        "type": "bytes32"
      }
    ],
    "name": "RoleAdminChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "sender",
        "type": "address"
      }
    ],
    "name": "RoleGranted",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "sender",
        "type": "address"
      }
    ],
    "name": "RoleRevoked",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "DEFAULT_ADMIN_ROLE",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes4",
        "name": "interfaceId",
        "type": "bytes4"
      }
    ],
    "name": "supportsInterface",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "hasRole",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      }
    ],
    "name": "getRoleAdmin",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "grantRole",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "revokeRole",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "role",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "renounceRole",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    UUPSProxiable: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "uuid",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "name": "CodeUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint8",
        "name": "version",
        "type": "uint8"
      }
    ],
    "name": "Initialized",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "getCodeAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "newAddress",
        "type": "address"
      }
    ],
    "name": "updateCode",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "castrate",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "proxiableUUID",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    IERC20: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    TokenInfo: [
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    ERC20WithTokenInfo: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    IResolver: [
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "target",
        "type": "address"
      }
    ],
    "name": "Set",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "address",
        "name": "target",
        "type": "address"
      }
    ],
    "name": "set",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      }
    ],
    "name": "get",
    "outputs": [
      {
        "internalType": "address",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    SuperfluidLoader: [
  {
    "inputs": [
      {
        "internalType": "contract IResolver",
        "name": "resolver",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "constructor"
  },
  {
    "inputs": [
      {
        "internalType": "string",
        "name": "releaseVersion",
        "type": "string"
      }
    ],
    "name": "loadFramework",
    "outputs": [
      {
        "components": [
          {
            "internalType": "contract ISuperfluid",
            "name": "superfluid",
            "type": "address"
          },
          {
            "internalType": "contract ISuperTokenFactory",
            "name": "superTokenFactory",
            "type": "address"
          },
          {
            "internalType": "contract ISuperAgreement",
            "name": "agreementCFAv1",
            "type": "address"
          },
          {
            "internalType": "contract ISuperAgreement",
            "name": "agreementIDAv1",
            "type": "address"
          }
        ],
        "internalType": "struct SuperfluidLoader.Framework",
        "name": "result",
        "type": "tuple"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    ISuperfluid: [
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "_code",
        "type": "uint256"
      }
    ],
    "name": "APP_RULE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_ALREADY_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_CALLBACK_IS_NOT_ACTION",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_IS_NOT_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CALL_AGREEMENT_WITH_CTX_FROM_WRONG_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CALL_APP_ACTION_WITH_CTX_FROM_WRONG_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CANNOT_DOWNGRADE_TO_NON_UPGRADEABLE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_INVALID_CONFIG_WORD",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_INVALID_OR_EXPIRED_SUPER_APP_REGISTRATION_KEY",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_MAX_256_AGREEMENTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_MUST_BE_CONTRACT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NEED_MORE_GAS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NON_UPGRADEABLE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NON_ZERO_LENGTH_PLACEHOLDER_CTX",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NOT_A_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NO_APP_REGISTRATION_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_ONLY_GOVERNANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_ONLY_LISTED_AGREEMENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_RECEIVER_IS_NOT_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SENDER_IS_NOT_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SOURCE_APP_NEEDS_HIGHER_APP_LEVEL",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SUPER_APP_ALREADY_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SUPER_APP_IS_JAILED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_UNAUTHORIZED_SUPER_APP_FACTORY",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_UNKNOWN_BATCH_CALL_OPERATION_TYPE",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "AgreementClassRegistered",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "AgreementClassUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "AppRegistered",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "contract ISuperfluidGovernance",
        "name": "oldGov",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "contract ISuperfluidGovernance",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "GovernanceReplaced",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "reason",
        "type": "uint256"
      }
    ],
    "name": "Jail",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "contract ISuperTokenFactory",
        "name": "newFactory",
        "type": "address"
      }
    ],
    "name": "SuperTokenFactoryUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "SuperTokenLogicUpdated",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "getNow",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getGovernance",
    "outputs": [
      {
        "internalType": "contract ISuperfluidGovernance",
        "name": "governance",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidGovernance",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "replaceGovernance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClassLogic",
        "type": "address"
      }
    ],
    "name": "registerAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClassLogic",
        "type": "address"
      }
    ],
    "name": "updateAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "isAgreementTypeListed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "yes",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "name": "isAgreementClassListed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "yes",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "getAgreementClass",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      }
    ],
    "name": "mapAgreementClasses",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement[]",
        "name": "agreementClasses",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      },
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "addToAgreementClassesBitmap",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "newBitmap",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      },
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "removeFromAgreementClassesBitmap",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "newBitmap",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenFactory",
    "outputs": [
      {
        "internalType": "contract ISuperTokenFactory",
        "name": "factory",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenFactoryLogic",
    "outputs": [
      {
        "internalType": "address",
        "name": "logic",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperTokenFactory",
        "name": "newFactory",
        "type": "address"
      }
    ],
    "name": "updateSuperTokenFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "updateSuperTokenLogic",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      }
    ],
    "name": "registerApp",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      },
      {
        "internalType": "string",
        "name": "registrationKey",
        "type": "string"
      }
    ],
    "name": "registerAppWithKey",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      }
    ],
    "name": "registerAppByFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "isApp",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "getAppCallbackLevel",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "appCallbackLevel",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "getAppManifest",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSuperApp",
        "type": "bool"
      },
      {
        "internalType": "bool",
        "name": "isJailed",
        "type": "bool"
      },
      {
        "internalType": "uint256",
        "name": "noopMask",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "isAppJailed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isJail",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "targetApp",
        "type": "address"
      }
    ],
    "name": "allowCompositeApp",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "targetApp",
        "type": "address"
      }
    ],
    "name": "isCompositeAppAllowed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isAppAllowed",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bool",
        "name": "isTermination",
        "type": "bool"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppBeforeCallback",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "cbdata",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bool",
        "name": "isTermination",
        "type": "bool"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppAfterCallback",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "appCreditGranted",
        "type": "uint256"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsed",
        "type": "int256"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "appCreditToken",
        "type": "address"
      }
    ],
    "name": "appCallbackPush",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsedDelta",
        "type": "int256"
      }
    ],
    "name": "appCallbackPop",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsedMore",
        "type": "int256"
      }
    ],
    "name": "ctxUseCredit",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "reason",
        "type": "uint256"
      }
    ],
    "name": "jailApp",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "callAgreement",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      }
    ],
    "name": "callAppAction",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAgreementWithContext",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppActionWithContext",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "decodeCtx",
    "outputs": [
      {
        "components": [
          {
            "internalType": "uint8",
            "name": "appCallbackLevel",
            "type": "uint8"
          },
          {
            "internalType": "uint8",
            "name": "callType",
            "type": "uint8"
          },
          {
            "internalType": "uint256",
            "name": "timestamp",
            "type": "uint256"
          },
          {
            "internalType": "address",
            "name": "msgSender",
            "type": "address"
          },
          {
            "internalType": "bytes4",
            "name": "agreementSelector",
            "type": "bytes4"
          },
          {
            "internalType": "bytes",
            "name": "userData",
            "type": "bytes"
          },
          {
            "internalType": "uint256",
            "name": "appCreditGranted",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "appCreditWantedDeprecated",
            "type": "uint256"
          },
          {
            "internalType": "int256",
            "name": "appCreditUsed",
            "type": "int256"
          },
          {
            "internalType": "address",
            "name": "appAddress",
            "type": "address"
          },
          {
            "internalType": "contract ISuperfluidToken",
            "name": "appCreditToken",
            "type": "address"
          }
        ],
        "internalType": "struct ISuperfluid.Context",
        "name": "context",
        "type": "tuple"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "isCtxValid",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint32",
            "name": "operationType",
            "type": "uint32"
          },
          {
            "internalType": "address",
            "name": "target",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "data",
            "type": "bytes"
          }
        ],
        "internalType": "struct ISuperfluid.Operation[]",
        "name": "operations",
        "type": "tuple[]"
      }
    ],
    "name": "batchCall",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint32",
            "name": "operationType",
            "type": "uint32"
          },
          {
            "internalType": "address",
            "name": "target",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "data",
            "type": "bytes"
          }
        ],
        "internalType": "struct ISuperfluid.Operation[]",
        "name": "operations",
        "type": "tuple[]"
      }
    ],
    "name": "forwardBatchCall",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    Superfluid: [
  {
    "inputs": [
      {
        "internalType": "bool",
        "name": "nonUpgradable",
        "type": "bool"
      },
      {
        "internalType": "bool",
        "name": "appWhiteListingEnabled",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "constructor"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "_code",
        "type": "uint256"
      }
    ],
    "name": "APP_RULE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_ALREADY_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_CALLBACK_IS_NOT_ACTION",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_AGREEMENT_IS_NOT_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CALL_AGREEMENT_WITH_CTX_FROM_WRONG_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CALL_APP_ACTION_WITH_CTX_FROM_WRONG_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_CANNOT_DOWNGRADE_TO_NON_UPGRADEABLE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_INVALID_CONFIG_WORD",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_INVALID_OR_EXPIRED_SUPER_APP_REGISTRATION_KEY",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_MAX_256_AGREEMENTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_MUST_BE_CONTRACT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NEED_MORE_GAS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NON_UPGRADEABLE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NON_ZERO_LENGTH_PLACEHOLDER_CTX",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NOT_A_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_NO_APP_REGISTRATION_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_ONLY_GOVERNANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_ONLY_LISTED_AGREEMENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_RECEIVER_IS_NOT_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SENDER_IS_NOT_SUPER_APP",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SOURCE_APP_NEEDS_HIGHER_APP_LEVEL",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SUPER_APP_ALREADY_REGISTERED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_SUPER_APP_IS_JAILED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_UNAUTHORIZED_SUPER_APP_FACTORY",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "HOST_UNKNOWN_BATCH_CALL_OPERATION_TYPE",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "AgreementClassRegistered",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "AgreementClassUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "AppRegistered",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "uuid",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "name": "CodeUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "contract ISuperfluidGovernance",
        "name": "oldGov",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "contract ISuperfluidGovernance",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "GovernanceReplaced",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint8",
        "name": "version",
        "type": "uint8"
      }
    ],
    "name": "Initialized",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "reason",
        "type": "uint256"
      }
    ],
    "name": "Jail",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "contract ISuperTokenFactory",
        "name": "newFactory",
        "type": "address"
      }
    ],
    "name": "SuperTokenFactoryUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "code",
        "type": "address"
      }
    ],
    "name": "SuperTokenLogicUpdated",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "APP_WHITE_LISTING_ENABLED",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CALLBACK_GAS_LIMIT",
    "outputs": [
      {
        "internalType": "uint64",
        "name": "",
        "type": "uint64"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "MAX_APP_CALLBACK_LEVEL",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "MAX_NUM_AGREEMENTS",
    "outputs": [
      {
        "internalType": "uint32",
        "name": "",
        "type": "uint32"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "NON_UPGRADABLE_DEPLOYMENT",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "castrate",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getCodeAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidGovernance",
        "name": "gov",
        "type": "address"
      }
    ],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "proxiableUUID",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "newAddress",
        "type": "address"
      }
    ],
    "name": "updateCode",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getNow",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getGovernance",
    "outputs": [
      {
        "internalType": "contract ISuperfluidGovernance",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidGovernance",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "replaceGovernance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClassLogic",
        "type": "address"
      }
    ],
    "name": "registerAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClassLogic",
        "type": "address"
      }
    ],
    "name": "updateAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "isAgreementTypeListed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "yes",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "name": "isAgreementClassListed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "yes",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "getAgreementClass",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      }
    ],
    "name": "mapAgreementClasses",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement[]",
        "name": "agreementClasses",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      },
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "addToAgreementClassesBitmap",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "newBitmap",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "bitmap",
        "type": "uint256"
      },
      {
        "internalType": "bytes32",
        "name": "agreementType",
        "type": "bytes32"
      }
    ],
    "name": "removeFromAgreementClassesBitmap",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "newBitmap",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenFactory",
    "outputs": [
      {
        "internalType": "contract ISuperTokenFactory",
        "name": "factory",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenFactoryLogic",
    "outputs": [
      {
        "internalType": "address",
        "name": "logic",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperTokenFactory",
        "name": "newFactory",
        "type": "address"
      }
    ],
    "name": "updateSuperTokenFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "updateSuperTokenLogic",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      }
    ],
    "name": "registerApp",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      },
      {
        "internalType": "string",
        "name": "registrationKey",
        "type": "string"
      }
    ],
    "name": "registerAppWithKey",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "configWord",
        "type": "uint256"
      }
    ],
    "name": "registerAppByFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "isApp",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "appAddr",
        "type": "address"
      }
    ],
    "name": "getAppCallbackLevel",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "getAppManifest",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSuperApp",
        "type": "bool"
      },
      {
        "internalType": "bool",
        "name": "isJailed",
        "type": "bool"
      },
      {
        "internalType": "uint256",
        "name": "noopMask",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      }
    ],
    "name": "isAppJailed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "targetApp",
        "type": "address"
      }
    ],
    "name": "allowCompositeApp",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "targetApp",
        "type": "address"
      }
    ],
    "name": "isCompositeAppAllowed",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bool",
        "name": "isTermination",
        "type": "bool"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppBeforeCallback",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "cbdata",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bool",
        "name": "isTermination",
        "type": "bool"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppAfterCallback",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "appCreditGranted",
        "type": "uint256"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsed",
        "type": "int256"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "appCreditToken",
        "type": "address"
      }
    ],
    "name": "appCallbackPush",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "appCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsedDelta",
        "type": "int256"
      }
    ],
    "name": "appCallbackPop",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "int256",
        "name": "appCreditUsedMore",
        "type": "int256"
      }
    ],
    "name": "ctxUseCredit",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      },
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "reason",
        "type": "uint256"
      }
    ],
    "name": "jailApp",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "callAgreement",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      }
    ],
    "name": "callAppAction",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperAgreement",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAgreementWithContext",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "returnedData",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperApp",
        "name": "app",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "callData",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "callAppActionWithContext",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "decodeCtx",
    "outputs": [
      {
        "components": [
          {
            "internalType": "uint8",
            "name": "appCallbackLevel",
            "type": "uint8"
          },
          {
            "internalType": "uint8",
            "name": "callType",
            "type": "uint8"
          },
          {
            "internalType": "uint256",
            "name": "timestamp",
            "type": "uint256"
          },
          {
            "internalType": "address",
            "name": "msgSender",
            "type": "address"
          },
          {
            "internalType": "bytes4",
            "name": "agreementSelector",
            "type": "bytes4"
          },
          {
            "internalType": "bytes",
            "name": "userData",
            "type": "bytes"
          },
          {
            "internalType": "uint256",
            "name": "appCreditGranted",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "appCreditWantedDeprecated",
            "type": "uint256"
          },
          {
            "internalType": "int256",
            "name": "appCreditUsed",
            "type": "int256"
          },
          {
            "internalType": "address",
            "name": "appAddress",
            "type": "address"
          },
          {
            "internalType": "contract ISuperfluidToken",
            "name": "appCreditToken",
            "type": "address"
          }
        ],
        "internalType": "struct ISuperfluid.Context",
        "name": "context",
        "type": "tuple"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "isCtxValid",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint32",
            "name": "operationType",
            "type": "uint32"
          },
          {
            "internalType": "address",
            "name": "target",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "data",
            "type": "bytes"
          }
        ],
        "internalType": "struct ISuperfluid.Operation[]",
        "name": "operations",
        "type": "tuple[]"
      }
    ],
    "name": "batchCall",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "uint32",
            "name": "operationType",
            "type": "uint32"
          },
          {
            "internalType": "address",
            "name": "target",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "data",
            "type": "bytes"
          }
        ],
        "internalType": "struct ISuperfluid.Operation[]",
        "name": "operations",
        "type": "tuple[]"
      }
    ],
    "name": "forwardBatchCall",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "forwarder",
        "type": "address"
      }
    ],
    "name": "isTrustedForwarder",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "versionRecipient",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  }
],
    ISuperToken: [
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_BURN_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_MOVE_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_LISTED_AGREEMENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_BURN_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_INFLATIONARY_DEFLATIONARY_NOT_SUPPORTED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_MINT_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NOT_ERC777_TOKENS_RECIPIENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NO_UNDERLYING_TOKEN",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_GOV_OWNER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_SELF",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "rewardAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "bondAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidatedBy",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "rewardAmountReceiver",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      }
    ],
    "name": "AgreementLiquidatedV2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      }
    ],
    "name": "AgreementStateUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      }
    ],
    "name": "AgreementTerminated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "AuthorizedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "bailoutAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "Bailout",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Burned",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantInflowNFT",
        "name": "constantInflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantInflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantOutflowNFT",
        "name": "constantOutflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantOutflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Minted",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "RevokedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Sent",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenDowngraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenUpgraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "createAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "getAccountActiveAgreements",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement[]",
        "name": "activeAgreements",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementData",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementStateSlot",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getHost",
    "outputs": [
      {
        "internalType": "address",
        "name": "host",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountCritical",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountCriticalNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountSolvent",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountSolventNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      },
      {
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "internalType": "bool",
        "name": "useDefaultRewardAccount",
        "type": "bool"
      },
      {
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      }
    ],
    "name": "makeLiquidationPayoutsV2",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "realtimeBalanceOfNow",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "int256",
        "name": "delta",
        "type": "int256"
      }
    ],
    "name": "settleBalance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "terminateAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementData",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementStateSlot",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract IERC20",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "underlyingDecimals",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "n",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "s",
        "type": "string"
      }
    ],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CONSTANT_OUTFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CONSTANT_INFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "balance",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "increaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "decreaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "granularity",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "send",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "burn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "isOperatorFor",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "authorizeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "revokeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "defaultOperators",
    "outputs": [
      {
        "internalType": "address[]",
        "name": "",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfMint",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfApproveFor",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      }
    ],
    "name": "transferAll",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getUnderlyingToken",
    "outputs": [
      {
        "internalType": "address",
        "name": "tokenAddr",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "upgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "upgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationApprove",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "operationIncreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "operationDecreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "operationSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationUpgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationDowngrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    SuperToken: [
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "constantOutflowNFT",
        "type": "address"
      },
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "constantInflowNFT",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "constructor"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_BURN_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_MOVE_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_LISTED_AGREEMENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_BURN_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_INFLATIONARY_DEFLATIONARY_NOT_SUPPORTED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_MINT_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NOT_ERC777_TOKENS_RECIPIENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NO_UNDERLYING_TOKEN",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_GOV_OWNER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_SELF",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "rewardAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "bondAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidatedBy",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "rewardAmountReceiver",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      }
    ],
    "name": "AgreementLiquidatedV2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      }
    ],
    "name": "AgreementStateUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      }
    ],
    "name": "AgreementTerminated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "AuthorizedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "bailoutAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "Bailout",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Burned",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "uuid",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "name": "CodeUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantInflowNFT",
        "name": "constantInflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantInflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantOutflowNFT",
        "name": "constantOutflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantOutflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint8",
        "name": "version",
        "type": "uint8"
      }
    ],
    "name": "Initialized",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Minted",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "RevokedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Sent",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenDowngraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenUpgraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "CONSTANT_INFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CONSTANT_OUTFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "castrate",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "createAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "getAccountActiveAgreements",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement[]",
        "name": "",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementData",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementStateSlot",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getCodeAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getHost",
    "outputs": [
      {
        "internalType": "address",
        "name": "host",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountCritical",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountCriticalNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountSolvent",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountSolventNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      },
      {
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "internalType": "bool",
        "name": "useDefaultRewardAccount",
        "type": "bool"
      },
      {
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      }
    ],
    "name": "makeLiquidationPayoutsV2",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "realtimeBalanceOfNow",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "int256",
        "name": "delta",
        "type": "int256"
      }
    ],
    "name": "settleBalance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "terminateAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementData",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementStateSlot",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract IERC20",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "underlyingDecimals",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "n",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "s",
        "type": "string"
      }
    ],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "proxiableUUID",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "newAddress",
        "type": "address"
      }
    ],
    "name": "updateCode",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "balance",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "holder",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "increaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "decreaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "granularity",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "send",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "burn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "isOperatorFor",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "authorizeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "revokeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "defaultOperators",
    "outputs": [
      {
        "internalType": "address[]",
        "name": "",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfMint",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfApproveFor",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "holder",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      }
    ],
    "name": "transferAll",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getUnderlyingToken",
    "outputs": [
      {
        "internalType": "address",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "upgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "upgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationApprove",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "operationIncreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "operationDecreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "operationSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationUpgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationDowngrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    ISuperTokenFactory: [
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_NON_UPGRADEABLE_IS_DEPRECATED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_UNINITIALIZED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "CustomSuperTokenCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "SuperTokenCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "tokenLogic",
        "type": "address"
      }
    ],
    "name": "SuperTokenLogicCreated",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "getHost",
    "outputs": [
      {
        "internalType": "address",
        "name": "host",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenLogic",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ERC20WithTokenInfo",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "enum ISuperTokenFactory.Upgradability",
        "name": "upgradability",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "symbol",
        "type": "string"
      }
    ],
    "name": "createERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract IERC20",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "underlyingDecimals",
        "type": "uint8"
      },
      {
        "internalType": "enum ISuperTokenFactory.Upgradability",
        "name": "upgradability",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "symbol",
        "type": "string"
      }
    ],
    "name": "createERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ERC20WithTokenInfo",
        "name": "_underlyingToken",
        "type": "address"
      }
    ],
    "name": "createCanonicalERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "_underlyingToken",
        "type": "address"
      }
    ],
    "name": "computeCanonicalERC20WrapperAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "superTokenAddress",
        "type": "address"
      },
      {
        "internalType": "bool",
        "name": "isDeployed",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "_underlyingTokenAddress",
        "type": "address"
      }
    ],
    "name": "getCanonicalERC20Wrapper",
    "outputs": [
      {
        "internalType": "address",
        "name": "superTokenAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "customSuperTokenProxy",
        "type": "address"
      }
    ],
    "name": "initializeCustomSuperToken",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    SuperTokenFactory: [
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperToken",
        "name": "superTokenLogic",
        "type": "address"
      },
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "constantOutflowNFT",
        "type": "address"
      },
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "constantInflowNFT",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "constructor"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_NON_UPGRADEABLE_IS_DEPRECATED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ONLY_GOVERNANCE_OWNER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_UNINITIALIZED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_FACTORY_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "uuid",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "name": "CodeUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "CustomSuperTokenCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "uint8",
        "name": "version",
        "type": "uint8"
      }
    ],
    "name": "Initialized",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "token",
        "type": "address"
      }
    ],
    "name": "SuperTokenCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperToken",
        "name": "tokenLogic",
        "type": "address"
      }
    ],
    "name": "SuperTokenLogicCreated",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "CONSTANT_INFLOW_NFT_LOGIC",
    "outputs": [
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CONSTANT_OUTFLOW_NFT_LOGIC",
    "outputs": [
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "_SUPER_TOKEN_LOGIC",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "castrate",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "_underlyingToken",
        "type": "address"
      }
    ],
    "name": "computeCanonicalERC20WrapperAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "superTokenAddress",
        "type": "address"
      },
      {
        "internalType": "bool",
        "name": "isDeployed",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ERC20WithTokenInfo",
        "name": "_underlyingToken",
        "type": "address"
      }
    ],
    "name": "createCanonicalERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ERC20WithTokenInfo",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "enum ISuperTokenFactory.Upgradability",
        "name": "upgradability",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "symbol",
        "type": "string"
      }
    ],
    "name": "createERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract IERC20",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "underlyingDecimals",
        "type": "uint8"
      },
      {
        "internalType": "enum ISuperTokenFactory.Upgradability",
        "name": "upgradability",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "symbol",
        "type": "string"
      }
    ],
    "name": "createERC20Wrapper",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "_underlyingTokenAddress",
        "type": "address"
      }
    ],
    "name": "getCanonicalERC20Wrapper",
    "outputs": [
      {
        "internalType": "address",
        "name": "superTokenAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getCodeAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "codeAddress",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getHost",
    "outputs": [
      {
        "internalType": "address",
        "name": "host",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getSuperTokenLogic",
    "outputs": [
      {
        "internalType": "contract ISuperToken",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "components": [
          {
            "internalType": "address",
            "name": "underlyingToken",
            "type": "address"
          },
          {
            "internalType": "address",
            "name": "superToken",
            "type": "address"
          }
        ],
        "internalType": "struct SuperTokenFactoryBase.InitializeData[]",
        "name": "_data",
        "type": "tuple[]"
      }
    ],
    "name": "initializeCanonicalWrapperSuperTokens",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "customSuperTokenProxy",
        "type": "address"
      }
    ],
    "name": "initializeCustomSuperToken",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "proxiableUUID",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "newAddress",
        "type": "address"
      }
    ],
    "name": "updateCode",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    ISuperAgreement: [
  {
    "inputs": [],
    "name": "agreementType",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "time",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "dynamicBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    ISuperfluidGovernance: [
  {
    "inputs": [],
    "name": "SF_GOV_ARRAYS_NOT_SAME_LENGTH",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_GOV_INVALID_LIQUIDATION_OR_PATRICIAN_PERIOD",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_GOV_MUST_BE_CONTRACT",
    "type": "error"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "replaceGovernance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "name": "registerAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "hostNewLogic",
        "type": "address"
      },
      {
        "internalType": "address[]",
        "name": "agreementClassNewLogics",
        "type": "address[]"
      },
      {
        "internalType": "address",
        "name": "superTokenFactoryNewLogic",
        "type": "address"
      }
    ],
    "name": "updateContracts",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperToken[]",
        "name": "tokens",
        "type": "address[]"
      }
    ],
    "name": "batchUpdateSuperTokenLogic",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "value",
        "type": "address"
      }
    ],
    "name": "setConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "setConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "clearConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "getConfigAsAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "value",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "getConfigAsUint256",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    SuperfluidGovernanceBase: [
  {
    "inputs": [],
    "name": "SF_GOV_ARRAYS_NOT_SAME_LENGTH",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_GOV_INVALID_LIQUIDATION_OR_PATRICIAN_PERIOD",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_GOV_MUST_BE_CONTRACT",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "factory",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "authorized",
        "type": "bool"
      }
    ],
    "name": "AppFactoryAuthorizationChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "deployer",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "string",
        "name": "appRegistrationKey",
        "type": "string"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "expirationTs",
        "type": "uint256"
      }
    ],
    "name": "AppRegistrationKeyChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "liquidationPeriod",
        "type": "uint256"
      }
    ],
    "name": "CFAv1LiquidationPeriodChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "ConfigChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "liquidationPeriod",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "patricianPeriod",
        "type": "uint256"
      }
    ],
    "name": "PPPConfigurationChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "rewardAddress",
        "type": "address"
      }
    ],
    "name": "RewardAddressChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "minimumDeposit",
        "type": "uint256"
      }
    ],
    "name": "SuperTokenMinimumDepositChanged",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "isKeySet",
        "type": "bool"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "forwarder",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bool",
        "name": "enabled",
        "type": "bool"
      }
    ],
    "name": "TrustedForwarderChanged",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "newGov",
        "type": "address"
      }
    ],
    "name": "replaceGovernance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      }
    ],
    "name": "registerAgreementClass",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "hostNewLogic",
        "type": "address"
      },
      {
        "internalType": "address[]",
        "name": "agreementClassNewLogics",
        "type": "address[]"
      },
      {
        "internalType": "address",
        "name": "superTokenFactoryNewLogic",
        "type": "address"
      }
    ],
    "name": "updateContracts",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperToken[]",
        "name": "tokens",
        "type": "address[]"
      }
    ],
    "name": "batchUpdateSuperTokenLogic",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperToken[]",
        "name": "tokens",
        "type": "address[]"
      },
      {
        "internalType": "uint256[]",
        "name": "minimumDeposits",
        "type": "uint256[]"
      }
    ],
    "name": "batchUpdateSuperTokenMinimumDeposit",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      },
      {
        "internalType": "address",
        "name": "value",
        "type": "address"
      }
    ],
    "name": "setConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "setConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "clearConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "getConfigAsAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "value",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "key",
        "type": "bytes32"
      }
    ],
    "name": "getConfigAsUint256",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "period",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "getRewardAddress",
    "outputs": [
      {
        "internalType": "address",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "rewardAddress",
        "type": "address"
      }
    ],
    "name": "setRewardAddress",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "clearRewardAddress",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "getPPPConfig",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "liquidationPeriod",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "patricianPeriod",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "liquidationPeriod",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "patricianPeriod",
        "type": "uint256"
      }
    ],
    "name": "setPPPConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "clearPPPConfig",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "getSuperTokenMinimumDeposit",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "setSuperTokenMinimumDeposit",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperToken",
        "name": "superToken",
        "type": "address"
      }
    ],
    "name": "clearSuperTokenMinimumDeposit",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "forwarder",
        "type": "address"
      }
    ],
    "name": "isTrustedForwarder",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "forwarder",
        "type": "address"
      }
    ],
    "name": "enableTrustedForwarder",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "contract ISuperfluidToken",
        "name": "superToken",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "forwarder",
        "type": "address"
      }
    ],
    "name": "disableTrustedForwarder",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "deployer",
        "type": "address"
      },
      {
        "internalType": "string",
        "name": "registrationKey",
        "type": "string"
      }
    ],
    "name": "verifyAppRegistrationKey",
    "outputs": [
      {
        "internalType": "bool",
        "name": "validNow",
        "type": "bool"
      },
      {
        "internalType": "uint256",
        "name": "expirationTs",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "deployer",
        "type": "address"
      },
      {
        "internalType": "string",
        "name": "registrationKey",
        "type": "string"
      },
      {
        "internalType": "uint256",
        "name": "expirationTs",
        "type": "uint256"
      }
    ],
    "name": "setAppRegistrationKey",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "deployer",
        "type": "address"
      },
      {
        "internalType": "string",
        "name": "registrationKey",
        "type": "string"
      }
    ],
    "name": "clearAppRegistrationKey",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "factory",
        "type": "address"
      }
    ],
    "name": "isAuthorizedAppFactory",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "factory",
        "type": "address"
      }
    ],
    "name": "authorizeAppFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluid",
        "name": "host",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "factory",
        "type": "address"
      }
    ],
    "name": "unauthorizeAppFactory",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    IConstantFlowAgreementV1: [
  {
    "inputs": [],
    "name": "CFA_ACL_FLOW_RATE_ALLOWANCE_EXCEEDED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_NO_NEGATIVE_ALLOWANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_NO_SENDER_CREATE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_NO_SENDER_FLOW_OPERATOR",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_NO_SENDER_UPDATE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_OPERATOR_NO_CREATE_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_OPERATOR_NO_DELETE_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_OPERATOR_NO_UPDATE_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ACL_UNCLEAN_PERMISSIONS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_DEPOSIT_TOO_BIG",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_FLOW_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_FLOW_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_FLOW_RATE_TOO_BIG",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_HOOK_OUT_OF_GAS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_INVALID_FLOW_RATE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_NON_CRITICAL_SENDER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_NO_SELF_FLOW",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ZERO_ADDRESS_RECEIVER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "CFA_ZERO_ADDRESS_SENDER",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint8",
        "name": "permissions",
        "type": "uint8"
      },
      {
        "indexed": false,
        "internalType": "int96",
        "name": "flowRateAllowance",
        "type": "int96"
      }
    ],
    "name": "FlowOperatorUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "indexed": false,
        "internalType": "int256",
        "name": "totalSenderFlowRate",
        "type": "int256"
      },
      {
        "indexed": false,
        "internalType": "int256",
        "name": "totalReceiverFlowRate",
        "type": "int256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "FlowUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      }
    ],
    "name": "FlowUpdatedExtension",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "time",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "dynamicBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "agreementType",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      }
    ],
    "name": "getMaximumFlowRateFromDeposit",
    "outputs": [
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      }
    ],
    "name": "getDepositRequiredForFlowRate",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isPatricianPeriodNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCurrentlyPatricianPeriod",
        "type": "bool"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isPatricianPeriod",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "permissions",
        "type": "uint8"
      },
      {
        "internalType": "int96",
        "name": "flowRateAllowance",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "updateFlowOperatorPermissions",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "addedFlowRateAllowance",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "increaseFlowRateAllowance",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "subtractedFlowRateAllowance",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "decreaseFlowRateAllowance",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "authorizeFlowOperatorWithFullControl",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "revokeFlowOperatorWithFullControl",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "flowOperator",
        "type": "address"
      }
    ],
    "name": "getFlowOperatorData",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "flowOperatorId",
        "type": "bytes32"
      },
      {
        "internalType": "uint8",
        "name": "permissions",
        "type": "uint8"
      },
      {
        "internalType": "int96",
        "name": "flowRateAllowance",
        "type": "int96"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "flowOperatorId",
        "type": "bytes32"
      }
    ],
    "name": "getFlowOperatorDataByID",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "permissions",
        "type": "uint8"
      },
      {
        "internalType": "int96",
        "name": "flowRateAllowance",
        "type": "int96"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "createFlow",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "createFlowByOperator",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "updateFlow",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "updateFlowByOperator",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      }
    ],
    "name": "getFlow",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "agreementId",
        "type": "bytes32"
      }
    ],
    "name": "getFlowByID",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "getAccountFlowInfo",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      },
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "getNetFlow",
    "outputs": [
      {
        "internalType": "int96",
        "name": "flowRate",
        "type": "int96"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "deleteFlow",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "receiver",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "deleteFlowByOperator",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    IInstantDistributionAgreementV1: [
  {
    "inputs": [],
    "name": "IDA_INDEX_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_INDEX_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_INDEX_SHOULD_GROW",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_OPERATION_NOT_ALLOWED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_SUBSCRIPTION_ALREADY_APPROVED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_SUBSCRIPTION_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_SUBSCRIPTION_IS_NOT_APPROVED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "IDA_ZERO_ADDRESS_SUBSCRIBER",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "IndexCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "IndexDistributionClaimed",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "IndexSubscribed",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "units",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "IndexUnitsUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "IndexUnsubscribed",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "oldIndexValue",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "newIndexValue",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "totalUnitsPending",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "totalUnitsApproved",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "IndexUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "SubscriptionApproved",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "SubscriptionDistributionClaimed",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "SubscriptionRevoked",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "indexed": false,
        "internalType": "uint128",
        "name": "units",
        "type": "uint128"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "SubscriptionUnitsUpdated",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "time",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "dynamicBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "agreementType",
    "outputs": [
      {
        "internalType": "bytes32",
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "createIndex",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      }
    ],
    "name": "getIndex",
    "outputs": [
      {
        "internalType": "bool",
        "name": "exist",
        "type": "bool"
      },
      {
        "internalType": "uint128",
        "name": "indexValue",
        "type": "uint128"
      },
      {
        "internalType": "uint128",
        "name": "totalUnitsApproved",
        "type": "uint128"
      },
      {
        "internalType": "uint128",
        "name": "totalUnitsPending",
        "type": "uint128"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "calculateDistribution",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "actualAmount",
        "type": "uint256"
      },
      {
        "internalType": "uint128",
        "name": "newIndexValue",
        "type": "uint128"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "uint128",
        "name": "indexValue",
        "type": "uint128"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "updateIndex",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "distribute",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "approveSubscription",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "revokeSubscription",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "internalType": "uint128",
        "name": "units",
        "type": "uint128"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "updateSubscription",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      }
    ],
    "name": "getSubscription",
    "outputs": [
      {
        "internalType": "bool",
        "name": "exist",
        "type": "bool"
      },
      {
        "internalType": "bool",
        "name": "approved",
        "type": "bool"
      },
      {
        "internalType": "uint128",
        "name": "units",
        "type": "uint128"
      },
      {
        "internalType": "uint256",
        "name": "pendingDistribution",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "agreementId",
        "type": "bytes32"
      }
    ],
    "name": "getSubscriptionByID",
    "outputs": [
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "bool",
        "name": "approved",
        "type": "bool"
      },
      {
        "internalType": "uint128",
        "name": "units",
        "type": "uint128"
      },
      {
        "internalType": "uint256",
        "name": "pendingDistribution",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      }
    ],
    "name": "listSubscriptions",
    "outputs": [
      {
        "internalType": "address[]",
        "name": "publishers",
        "type": "address[]"
      },
      {
        "internalType": "uint32[]",
        "name": "indexIds",
        "type": "uint32[]"
      },
      {
        "internalType": "uint128[]",
        "name": "unitsList",
        "type": "uint128[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "deleteSubscription",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract ISuperfluidToken",
        "name": "token",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "publisher",
        "type": "address"
      },
      {
        "internalType": "uint32",
        "name": "indexId",
        "type": "uint32"
      },
      {
        "internalType": "address",
        "name": "subscriber",
        "type": "address"
      },
      {
        "internalType": "bytes",
        "name": "ctx",
        "type": "bytes"
      }
    ],
    "name": "claim",
    "outputs": [
      {
        "internalType": "bytes",
        "name": "newCtx",
        "type": "bytes"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
    TestToken: [
  {
    "inputs": [
      {
        "internalType": "string",
        "name": "name",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "symbol",
        "type": "string"
      },
      {
        "internalType": "uint8",
        "name": "initDecimals",
        "type": "uint8"
      },
      {
        "internalType": "uint256",
        "name": "mintLimit",
        "type": "uint256"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "constructor"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "decreaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "increaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "mint",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  }
],
    ISETH: [
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_ALREADY_EXISTS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_AGREEMENT_DOES_NOT_EXIST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_BURN_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_MOVE_INSUFFICIENT_BALANCE",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SF_TOKEN_ONLY_LISTED_AGREEMENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_APPROVE_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_BURN_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_CALLER_IS_NOT_OPERATOR_FOR_HOLDER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_INFLATIONARY_DEFLATIONARY_NOT_SUPPORTED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_MINT_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NFT_PROXY_ADDRESS_CHANGED",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NOT_ERC777_TOKENS_RECIPIENT",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_NO_UNDERLYING_TOKEN",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_GOV_OWNER",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_HOST",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_ONLY_SELF",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_FROM_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "inputs": [],
    "name": "SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS",
    "type": "error"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "rewardAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": false,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "penaltyAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "bondAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "AgreementLiquidatedBy",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "address",
        "name": "rewardAmountReceiver",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      }
    ],
    "name": "AgreementLiquidatedV2",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      }
    ],
    "name": "AgreementStateUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      }
    ],
    "name": "AgreementTerminated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "indexed": false,
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "AgreementUpdated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Approval",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "AuthorizedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "bailoutAccount",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "bailoutAmount",
        "type": "uint256"
      }
    ],
    "name": "Bailout",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Burned",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantInflowNFT",
        "name": "constantInflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantInflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "contract IConstantOutflowNFT",
        "name": "constantOutflowNFT",
        "type": "address"
      }
    ],
    "name": "ConstantOutflowNFTCreated",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Minted",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "RevokedOperator",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "indexed": false,
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "Sent",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenDowngraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "TokenUpgraded",
    "type": "event"
  },
  {
    "anonymous": false,
    "inputs": [
      {
        "indexed": true,
        "internalType": "address",
        "name": "from",
        "type": "address"
      },
      {
        "indexed": true,
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "indexed": false,
        "internalType": "uint256",
        "name": "value",
        "type": "uint256"
      }
    ],
    "name": "Transfer",
    "type": "event"
  },
  {
    "inputs": [],
    "name": "CONSTANT_INFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantInflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "CONSTANT_OUTFLOW_NFT",
    "outputs": [
      {
        "internalType": "contract IConstantOutflowNFT",
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "owner",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      }
    ],
    "name": "allowance",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "approve",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "authorizeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "balanceOf",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "balance",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "burn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "createAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "decimals",
    "outputs": [
      {
        "internalType": "uint8",
        "name": "",
        "type": "uint8"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "decreaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "defaultOperators",
    "outputs": [
      {
        "internalType": "address[]",
        "name": "",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "downgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "wad",
        "type": "uint256"
      }
    ],
    "name": "downgradeToETH",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "getAccountActiveAgreements",
    "outputs": [
      {
        "internalType": "contract ISuperAgreement[]",
        "name": "activeAgreements",
        "type": "address[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementData",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "agreementClass",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "getAgreementStateSlot",
    "outputs": [
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getHost",
    "outputs": [
      {
        "internalType": "address",
        "name": "host",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "getUnderlyingToken",
    "outputs": [
      {
        "internalType": "address",
        "name": "tokenAddr",
        "type": "address"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "granularity",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "increaseAllowance",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "contract IERC20",
        "name": "underlyingToken",
        "type": "address"
      },
      {
        "internalType": "uint8",
        "name": "underlyingDecimals",
        "type": "uint8"
      },
      {
        "internalType": "string",
        "name": "n",
        "type": "string"
      },
      {
        "internalType": "string",
        "name": "s",
        "type": "string"
      }
    ],
    "name": "initialize",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountCritical",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountCriticalNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isCritical",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "isAccountSolvent",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "isAccountSolventNow",
    "outputs": [
      {
        "internalType": "bool",
        "name": "isSolvent",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "tokenHolder",
        "type": "address"
      }
    ],
    "name": "isOperatorFor",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes",
        "name": "liquidationTypeData",
        "type": "bytes"
      },
      {
        "internalType": "address",
        "name": "liquidatorAccount",
        "type": "address"
      },
      {
        "internalType": "bool",
        "name": "useDefaultRewardAccount",
        "type": "bool"
      },
      {
        "internalType": "address",
        "name": "targetAccount",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "rewardAmount",
        "type": "uint256"
      },
      {
        "internalType": "int256",
        "name": "targetAccountBalanceDelta",
        "type": "int256"
      }
    ],
    "name": "makeLiquidationPayoutsV2",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "name",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationApprove",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "subtractedValue",
        "type": "uint256"
      }
    ],
    "name": "operationDecreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationDowngrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "addedValue",
        "type": "uint256"
      }
    ],
    "name": "operationIncreaseAllowance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "operationSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "operationUpgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      },
      {
        "internalType": "bytes",
        "name": "operatorData",
        "type": "bytes"
      }
    ],
    "name": "operatorSend",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "name": "realtimeBalanceOf",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      }
    ],
    "name": "realtimeBalanceOfNow",
    "outputs": [
      {
        "internalType": "int256",
        "name": "availableBalance",
        "type": "int256"
      },
      {
        "internalType": "uint256",
        "name": "deposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "owedDeposit",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "timestamp",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "operator",
        "type": "address"
      }
    ],
    "name": "revokeOperator",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfApproveFor",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfBurn",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "userData",
        "type": "bytes"
      }
    ],
    "name": "selfMint",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "spender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "selfTransferFrom",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "send",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "int256",
        "name": "delta",
        "type": "int256"
      }
    ],
    "name": "settleBalance",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "symbol",
    "outputs": [
      {
        "internalType": "string",
        "name": "",
        "type": "string"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "uint256",
        "name": "dataLength",
        "type": "uint256"
      }
    ],
    "name": "terminateAgreement",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "totalSupply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transfer",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      }
    ],
    "name": "transferAll",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "sender",
        "type": "address"
      },
      {
        "internalType": "address",
        "name": "recipient",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "transferFrom",
    "outputs": [
      {
        "internalType": "bool",
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "bytes32",
        "name": "id",
        "type": "bytes32"
      },
      {
        "internalType": "bytes32[]",
        "name": "data",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementData",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "account",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "slotId",
        "type": "uint256"
      },
      {
        "internalType": "bytes32[]",
        "name": "slotData",
        "type": "bytes32[]"
      }
    ],
    "name": "updateAgreementStateSlot",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      }
    ],
    "name": "upgrade",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "upgradeByETH",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      }
    ],
    "name": "upgradeByETHTo",
    "outputs": [],
    "stateMutability": "payable",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "address",
        "name": "to",
        "type": "address"
      },
      {
        "internalType": "uint256",
        "name": "amount",
        "type": "uint256"
      },
      {
        "internalType": "bytes",
        "name": "data",
        "type": "bytes"
      }
    ],
    "name": "upgradeTo",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
],
};
