Contracts = {
    IERC20: [{"anonymous":false,"inputs":[{"indexed":true,"internalType":"address","name":"owner","type":"address"},{"indexed":true,"internalType":"address","name":"spender","type":"address"},{"indexed":false,"internalType":"uint256","name":"value","type":"uint256"}],"name":"Approval","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"address","name":"from","type":"address"},{"indexed":true,"internalType":"address","name":"to","type":"address"},{"indexed":false,"internalType":"uint256","name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"inputs":[],"name":"totalSupply","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"address","name":"account","type":"address"}],"name":"balanceOf","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"address","name":"recipient","type":"address"},{"internalType":"uint256","name":"amount","type":"uint256"}],"name":"transfer","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"address","name":"owner","type":"address"},{"internalType":"address","name":"spender","type":"address"}],"name":"allowance","outputs":[{"internalType":"uint256","name":"","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"address","name":"spender","type":"address"},{"internalType":"uint256","name":"amount","type":"uint256"}],"name":"approve","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"address","name":"sender","type":"address"},{"internalType":"address","name":"recipient","type":"address"},{"internalType":"uint256","name":"amount","type":"uint256"}],"name":"transferFrom","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"nonpayable","type":"function"}],
    ISuperfluid: [{"anonymous":false,"inputs":[{"indexed":false,"internalType":"contract ISuperApp","name":"app","type":"address"},{"indexed":false,"internalType":"uint256","name":"info","type":"uint256"}],"name":"Jail","type":"event"},{"inputs":[],"name":"getGovernance","outputs":[{"internalType":"contract ISuperfluidGovernance","name":"governance","type":"address"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"getSuperTokenLogic","outputs":[{"internalType":"contract ISuperToken","name":"superToken","type":"address"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract IERC20","name":"underlyingToken","type":"address"},{"internalType":"string","name":"symbol","type":"string"}],"name":"getERC20Wrapper","outputs":[{"internalType":"address","name":"wrapperAddress","type":"address"},{"internalType":"bool","name":"created","type":"bool"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract IERC20","name":"underlyingToken","type":"address"},{"internalType":"uint8","name":"underlyingDecimals","type":"uint8"},{"internalType":"string","name":"name","type":"string"},{"internalType":"string","name":"symbol","type":"string"}],"name":"createERC20Wrapper","outputs":[{"internalType":"contract ISuperToken","name":"superToken","type":"address"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"uint256","name":"configWord","type":"uint256"}],"name":"registerApp","outputs":[],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"}],"name":"isApp","outputs":[{"internalType":"bool","name":"","type":"bool"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"}],"name":"getAppLevel","outputs":[{"internalType":"uint8","name":"appLevel","type":"uint8"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"}],"name":"getAppManifest","outputs":[{"internalType":"bool","name":"exist","type":"bool"},{"internalType":"uint256","name":"configWord","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"}],"name":"isAppJailed","outputs":[{"internalType":"bool","name":"isJail","type":"bool"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"targetApp","type":"address"}],"name":"allowCompositeApp","outputs":[],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"},{"internalType":"contract ISuperApp","name":"targetApp","type":"address"}],"name":"isCompositeAppAllowed","outputs":[{"internalType":"bool","name":"isAppAllowed","type":"bool"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"},{"internalType":"bool","name":"isTermination","type":"bool"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"callAppBeforeCallback","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"},{"internalType":"bytes","name":"cbdata","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"},{"internalType":"bool","name":"isTermination","type":"bool"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"callAppAfterCallback","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"bytes","name":"ctx","type":"bytes"},{"internalType":"uint8","name":"appLevel","type":"uint8"},{"internalType":"uint256","name":"allowance","type":"uint256"},{"internalType":"uint256","name":"allowanceUsed","type":"uint256"}],"name":"ctxUpdate","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperAgreement","name":"agreementClass","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"}],"name":"callAgreement","outputs":[{"internalType":"bytes","name":"returnedData","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"}],"name":"callAppAction","outputs":[{"internalType":"bytes","name":"returnedData","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"components":[{"internalType":"enum ISuperfluid.OperationType","name":"opType","type":"uint8"},{"internalType":"address","name":"target","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"}],"internalType":"struct ISuperfluid.Operation[]","name":"operations","type":"tuple[]"}],"name":"batchCall","outputs":[],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperAgreement","name":"agreementClass","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"callAgreementWithContext","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"},{"internalType":"bytes","name":"returnedData","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperApp","name":"app","type":"address"},{"internalType":"bytes","name":"data","type":"bytes"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"callAppActionWithContext","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"bytes","name":"ctx","type":"bytes"},{"internalType":"uint256","name":"fee","type":"uint256"}],"name":"chargeGasFee","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"decodeCtx","outputs":[{"internalType":"bytes4","name":"agreementSelector","type":"bytes4"},{"internalType":"uint8","name":"appLevel","type":"uint8"},{"internalType":"address","name":"msgSender","type":"address"},{"internalType":"uint256","name":"allowance","type":"uint256"},{"internalType":"uint256","name":"allowanceUsed","type":"uint256"}],"stateMutability":"pure","type":"function"}],
    IInstantDistributionAgreementV1: [{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"publisher","type":"address"},{"indexed":true,"internalType":"uint32","name":"indexId","type":"uint32"}],"name":"IndexCreated","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"publisher","type":"address"},{"indexed":true,"internalType":"uint32","name":"indexId","type":"uint32"},{"indexed":false,"internalType":"address","name":"subscriber","type":"address"}],"name":"IndexSubscribed","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"publisher","type":"address"},{"indexed":true,"internalType":"uint32","name":"indexId","type":"uint32"},{"indexed":false,"internalType":"address","name":"subscriber","type":"address"},{"indexed":false,"internalType":"uint128","name":"units","type":"uint128"}],"name":"IndexUnitsUpdated","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"publisher","type":"address"},{"indexed":true,"internalType":"uint32","name":"indexId","type":"uint32"},{"indexed":false,"internalType":"address","name":"subscriber","type":"address"}],"name":"IndexUnsubscribed","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"publisher","type":"address"},{"indexed":true,"internalType":"uint32","name":"indexId","type":"uint32"},{"indexed":false,"internalType":"uint128","name":"indexValue","type":"uint128"},{"indexed":false,"internalType":"uint128","name":"totalUnitsPending","type":"uint128"},{"indexed":false,"internalType":"uint128","name":"totalUnitsApproved","type":"uint128"}],"name":"IndexUpdated","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"subscriber","type":"address"},{"indexed":false,"internalType":"address","name":"publisher","type":"address"},{"indexed":false,"internalType":"uint32","name":"indexId","type":"uint32"}],"name":"SubscriptionApproved","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"subscriber","type":"address"},{"indexed":false,"internalType":"address","name":"publisher","type":"address"},{"indexed":false,"internalType":"uint32","name":"indexId","type":"uint32"}],"name":"SubscriptionDeleted","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"internalType":"contract ISuperToken","name":"token","type":"address"},{"indexed":true,"internalType":"address","name":"subscriber","type":"address"},{"indexed":false,"internalType":"address","name":"publisher","type":"address"},{"indexed":false,"internalType":"uint32","name":"indexId","type":"uint32"},{"indexed":false,"internalType":"uint128","name":"units","type":"uint128"}],"name":"SubscriptionUnitsUpdated","type":"event"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"account","type":"address"},{"internalType":"uint256","name":"time","type":"uint256"}],"name":"realtimeBalanceOf","outputs":[{"internalType":"int256","name":"dynamicBalance","type":"int256"},{"internalType":"uint256","name":"deposit","type":"uint256"},{"internalType":"uint256","name":"owedDeposit","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[],"name":"agreementType","outputs":[{"internalType":"bytes32","name":"","type":"bytes32"}],"stateMutability":"pure","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"createIndex","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"}],"name":"getIndex","outputs":[{"internalType":"bool","name":"exist","type":"bool"},{"internalType":"uint128","name":"indexValue","type":"uint128"},{"internalType":"uint128","name":"totalUnitsApproved","type":"uint128"},{"internalType":"uint128","name":"totalUnitsPending","type":"uint128"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"uint128","name":"indexValue","type":"uint128"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"updateIndex","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"uint256","name":"amount","type":"uint256"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"distribute","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"uint256","name":"amount","type":"uint256"}],"name":"calculateDistribution","outputs":[{"internalType":"uint256","name":"actualAmount","type":"uint256"},{"internalType":"uint128","name":"newIndexValue","type":"uint128"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"approveSubscription","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"address","name":"subscriber","type":"address"},{"internalType":"uint128","name":"units","type":"uint128"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"updateSubscription","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"address","name":"subscriber","type":"address"}],"name":"getSubscription","outputs":[{"internalType":"bool","name":"approved","type":"bool"},{"internalType":"uint128","name":"units","type":"uint128"},{"internalType":"uint256","name":"pendingDistribution","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"bytes32","name":"agreementId","type":"bytes32"}],"name":"getSubscriptionByID","outputs":[{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"bool","name":"approved","type":"bool"},{"internalType":"uint128","name":"units","type":"uint128"},{"internalType":"uint256","name":"pendingDistribution","type":"uint256"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"subscriber","type":"address"}],"name":"listSubscriptions","outputs":[{"internalType":"address[]","name":"publishers","type":"address[]"},{"internalType":"uint32[]","name":"indexIds","type":"uint32[]"},{"internalType":"uint128[]","name":"unitsList","type":"uint128[]"}],"stateMutability":"view","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"address","name":"subscriber","type":"address"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"deleteSubscription","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"},{"inputs":[{"internalType":"contract ISuperToken","name":"token","type":"address"},{"internalType":"address","name":"publisher","type":"address"},{"internalType":"uint32","name":"indexId","type":"uint32"},{"internalType":"bytes","name":"ctx","type":"bytes"}],"name":"claim","outputs":[{"internalType":"bytes","name":"newCtx","type":"bytes"}],"stateMutability":"nonpayable","type":"function"}],
    DividendRightsToken: [
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
            "internalType": "contract ISuperToken",
            "name": "cashToken",
            "type": "address"
          },
          {
            "internalType": "contract ISuperfluid",
            "name": "host",
            "type": "address"
          },
          {
            "internalType": "contract IInstantDistributionAgreementV1",
            "name": "ida",
            "type": "address"
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
        "name": "INDEX_ID",
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
        "inputs": [
          {
            "internalType": "contract ISuperToken",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "",
            "type": "bytes32"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          }
        ],
        "name": "afterAgreementTerminated",
        "outputs": [
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
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
            "internalType": "contract ISuperToken",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "",
            "type": "bytes32"
          }
        ],
        "name": "beforeAgreementTerminated",
        "outputs": [
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
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
        "name": "isOwner",
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
            "name": "",
            "type": "address"
          }
        ],
        "name": "isSubscribing",
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
            "name": "newOwner",
            "type": "address"
          }
        ],
        "name": "transferOwnership",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "contract ISuperToken",
            "name": "superToken",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "agreementClass",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "",
            "type": "bytes32"
          }
        ],
        "name": "beforeAgreementCreated",
        "outputs": [
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "contract ISuperToken",
            "name": "superToken",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "ctx",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "agreementId",
            "type": "bytes32"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          }
        ],
        "name": "afterAgreementCreated",
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
            "internalType": "contract ISuperToken",
            "name": "superToken",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "agreementClass",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "",
            "type": "bytes32"
          }
        ],
        "name": "beforeAgreementUpdated",
        "outputs": [
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "contract ISuperToken",
            "name": "superToken",
            "type": "address"
          },
          {
            "internalType": "bytes",
            "name": "ctx",
            "type": "bytes"
          },
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          },
          {
            "internalType": "bytes32",
            "name": "agreementId",
            "type": "bytes32"
          },
          {
            "internalType": "bytes",
            "name": "",
            "type": "bytes"
          }
        ],
        "name": "afterAgreementUpdated",
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
            "internalType": "address",
            "name": "beneficiary",
            "type": "address"
          },
          {
            "internalType": "uint256",
            "name": "amount",
            "type": "uint256"
          }
        ],
        "name": "issue",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "cashAmount",
            "type": "uint256"
          }
        ],
        "name": "distribute",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      }
    ]
};
