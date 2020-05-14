pragma solidity 0.6.6;

import { IERC20 } from "./interface/IERC20.sol";
import { ISuperToken } from "./interface/ISuperToken.sol";

import "./ERC20Base.sol";
import "./interface/ISuperAgreement.sol";

contract SuperToken is ISuperToken, ERC20Base {
    /// Underlaying ERC20 token
    IERC20 private _token;

    /*
     * Example:
     *
     * - a1/agreement data 1: Fran -> Nuno: 10 DAI / mo
     * - a2/agreement data 2: Miao -> Nuno: 10 DAI / mo
     * - a3/agreement data 3: Mike -> Nuno: 10 DAI / mo
     *
     * 3 agreements data:
     * a1: sha3(flowAgreement, sha3(Fran, Nuno)) -> (10 DAI / mo)
     * a2: sha3(flowAgreement, sha3(Miao, Nuno)) -> (10 DAI / mo)
     * a3: sha3(flowAgreement, sha3(Mike, Nuno)) -> (10 DAI / mo)
     *
     *
     *
     *
     * 4 agreement account STATES:
     * sha3(flowAgreement, account) -> state
     *
     * Fran: outflow 10 dai / mo, inflow 0 dai / mo
     * Miao: outflow 10 dai / mo, inflow 0 dai / mo
     * Mike: outflow 10 dai / mo, inflow 0 dai / mo
     * Nuno: outflow 0 dai / mo, inflow 30 dai / mo
     */

    /// Mapping from sha3(agreementClass, agreementID) to agreement data
    /// the generation of agreementID is the logic of agreement contract
    mapping(bytes32 => bytes) private _agreementData;

    /// Mapping from account to agreement state of the account
    /// It is like RUNTIME state of the agreement for each account
    mapping(address => bytes) private _agreementAccountStates;

    /// List of enabled agreement classes for the account
    mapping(address => address[]) private _accountActiveAgreementClasses;

    /// Settled balance for the account
    mapping(address => int256) private _settledBalances;

    constructor (IERC20 token, string memory name, string memory symbol)
    public
    ERC20Base(name, symbol) {
        _token = token;
    }

     /// @dev ISuperToken.balanceOf implementation
    function balanceOf(
        address account
    )
        public
        view
        returns(uint256 balance)
    {
         // can return negative
        (uint256 _balance,) = _calculateBalance(account, block.timestamp);
        return _balance;
    }

    /// @notice Calculate the real balance of a user, taking in consideration all flows of tokens
    ///         Used by solvency agent
    /// @param account User to calculate balance
    /// @param timestamp Time of balance
    /// @return balance Account balance
    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
    public
    view
    returns (int256)
    {
        // can return negative
        (, int256 _balance) = _calculateBalance(account, timestamp);
        return _balance;
    }


    /*
    *   Agreement Account States
    */

    /// @dev ISuperToken.getAgreementAccountState implementation
    function getAgreementAccountState(
        address account
    )
        external
        view
        override
        returns (bytes memory data)
    {
        return _agreementAccountStates[account];
    }

    /// @dev ISuperToken.updateAgreementAccountState implementation
    function updateAgreementAccountState(
        address account,
        bytes calldata state
    )
        external
        override
    {
        _agreementAccountStates[account] = state;
    }


    /*
    * Agreement Data
    */

    /// @dev ISuperToken.createAgreement implementation
    function createAgreement(
        address agreementClass,
        bytes32 id,
        bytes calldata data
    ) 
        external
        override
    {
       _agreementData[_agreementDataId(agreementClass, id)] = data;
    }

    /// @dev ISuperToken.getAgreementData implementation
    function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        view
        override
        returns(bytes memory state)
    {
        return _agreementData[_agreementDataId(agreementClass, id)];
    }
    /// @dev ISuperToken.terminateAgreement implementation
    function terminateAgreement(
        address agreementClass,
        bytes32 id
    )
        external
        override
    {
        _takeBalanceSnapshot(msg.sender);
        delete _agreementData[_agreementDataId(agreementClass, id)];
        _removeActiveAgreement(agreementClass, msg.sender);
    }

    /*
    * SuperToken 
    */

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        require(uint256(balanceOf(msg.sender)) >= amount, "amount not allowed");

        _touch(msg.sender);
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
    }


    /*
    *  Internal functions
    */

     /// @dev Calculate balance as split result if negative return as zero.
    function _calculateBalance(address account, uint256 timestamp) internal view returns(uint256, int256) {
        int256 _eachAgreementClassBalance;
        int256 _balance;
        address _agreementClass;

        for(uint256 i = 0; i < _accountActiveAgreementClasses[account].length; i++)  {
            _agreementClass = _accountActiveAgreementClasses[account][i];
            _eachAgreementClassBalance += ISuperAgreement(_agreementClass).balanceOf(
                _agreementAccountStates[account], timestamp
            );
        }

        _balance = _settledBalances[account] + _eachAgreementClassBalance + int256(_balances[account]);
        return _balance < 0 ? (0, _balance) : (uint256(_balance), _balance);
    }


    /// @notice for each receiving flow, lets set the timestamp to `now`, making a partial settlement
    /// TODO: Let think about how we are getting the idAgreement
    function _touch(address account) internal {
        address _agreementClass;
        bytes memory _touchState;
        int256 _balance = realtimeBalanceOf(account, block.timestamp) - int256(_balances[account]);

        for(uint256 i = 0; i < _accountActiveAgreementClasses[account].length; i++) {

            _agreementClass = _accountActiveAgreementClasses[account][i];
            _touchState = ISuperAgreement(_agreementClass).touch(_agreementAccountStates[account], block.timestamp);
            _agreementAccountStates[account] = _touchState;
            _settledBalances[account] = 0;
        }

        if(_balance > 0) {
            _mint(account, uint256(_balance));
        }
    }


     /// @dev if returns -1 agreement is not active
    function _indexOfActiveAgreement(address agreementClass, address account) internal view returns(int256) {
        int256 i;
        int256 _size = int256(_accountActiveAgreementClasses[account].length);

        while (i < _size) {

            if (_accountActiveAgreementClasses[account][uint256(i)] == agreementClass) {
                return i;
            }

            i++;
        }

        return -1;
    }


    function _removeActiveAgreement(address agreementClass, address account) internal {
        int256 _idx = _indexOfActiveAgreement(agreementClass, account);
        uint256 _size = _accountActiveAgreementClasses[account].length;

        //If we have a valid index then that element can be removed
        if (_idx >= 0) {
            if (_size - 1 == uint256(_idx)) {
                _accountActiveAgreementClasses[account].pop();
            } else {
                //swap element and pop
                _accountActiveAgreementClasses[account][uint256(_idx)] = _accountActiveAgreementClasses[account][_size - 1];
                _accountActiveAgreementClasses[account].pop();
            }
        }
    }

    /// @dev Save the balance until now
    /// @param account User to snapshot balance
    function _takeBalanceSnapshot(address account) internal {
        _settledBalances[account] = realtimeBalanceOf(account, block.timestamp);
    }

    /// @dev Hash agreement with accounts
    function _agreementDataId(address agreementClass, bytes32 accounts) public pure returns(bytes32) {
        return keccak256(abi.encodePacked(agreementClass,accounts));
    }
}
