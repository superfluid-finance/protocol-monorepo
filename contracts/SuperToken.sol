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
    /// the generation of agreementDataID is the logic of agreement contract
    mapping(bytes32 => bytes) private _agreementData;

    /// Mapping from account to agreement state of the account
    /// It is like RUNTIME state of the agreement for each account
    mapping(address => bytes) private _agreementAccountStates;

    /// List of enabled agreement classes for the account
    mapping(address => address[]) public accountActiveAgreementClasses;

    /// Settled balance for the account
    mapping(address => int256) private _settledBalances;

    constructor (IERC20 token, string memory name, string memory symbol)
    public
    ERC20Base(name, symbol) {
        _token = token;
    }

    function getAccountActiveAgreements(address account) public view returns(address[] memory) {
        return accountActiveAgreementClasses[account];
    }

    /// @dev ISuperToken.balanceOf implementation
    function balanceOf(
        address account
    )
        public
        view
        returns(uint256 balance)
    {
        (int256 _balance) = _calculateBalance(account, block.timestamp);
        return _balance < 0 ? 0 : uint256(_balance);
    }

    /// review ok
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
        return _calculateBalance(account, timestamp);
    }


    /*
    *   Agreement Account States
    */

   //review ok
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
        require(msg.sender != account, "Use the agreement contract");
        _takeBalanceSnapshot(account);
        _agreementAccountStates[account] = state;
        state.length != 0 ? _addAgreementClass(msg.sender, account) : _delAgreementClass(msg.sender, account);
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

    //review - let dig a little more
    /// @dev ISuperToken.terminateAgreement implementation
    function terminateAgreement(
        address agreementClass,
        bytes32 id
    )
        external
        override
    {
        delete _agreementData[_agreementDataId(agreementClass, id)];
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
        //review TODO touch only need, by the requirement amount
        _touch(msg.sender);
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
    }

    function getSettledBalance(address account) external view returns(int256 settledBalance) {
       return _settledBalances[account];
    }


    /*
    *  Internal functions
    */


    /// @dev Calculate balance as split result if negative return as zero.
    function _calculateBalance(address account, uint256 timestamp) internal view returns(int256) {

        int256 _eachAgreementClassBalance;
        address _agreementClass;

        for(uint256 i = 0; i < accountActiveAgreementClasses[account].length; i++)  {
            _agreementClass = accountActiveAgreementClasses[account][i];
            _eachAgreementClassBalance +=
                ISuperAgreement(_agreementClass).balanceOf(_agreementAccountStates[account], timestamp);
        }

        return _settledBalances[account] + _eachAgreementClassBalance + int256(_balances[account]);
    }


    /// @notice for each receiving flow, lets set the timestamp to `now`, making a partial settlement
    function _touch(address account) internal {

        address _agreementClass;
        bytes memory _touchState;
        int256 _balance = realtimeBalanceOf(account, block.timestamp) - int256(_balances[account]);

        for(uint256 i = 0; i < accountActiveAgreementClasses[account].length; i++) {

            _agreementClass = accountActiveAgreementClasses[account][i];
            _touchState = ISuperAgreement(_agreementClass).touch(_agreementAccountStates[account], block.timestamp);

            _agreementAccountStates[account] = _touchState;
        }

        _settledBalances[account] = 0;
        if(_balance > 0) {
            _mint(account, uint256(_balance));
        }
    }


    /// @dev if returns -1 agreement is not active
    function _indexOfAgreementClass(address agreementClass, address account) internal view returns(int256) {

        int256 i;
        int256 _size = int256(accountActiveAgreementClasses[account].length);

        while (i < _size) {

            if (accountActiveAgreementClasses[account][uint256(i)] == agreementClass) {
                return i;
            }

            i++;
        }

        return -1;
    }

    function _delAgreementClass(address agreementClass, address account) internal {

        int256 _idx = _indexOfAgreementClass(agreementClass, account);
        uint256 _size = accountActiveAgreementClasses[account].length;

        if (_idx >= 0) {

            if (_size - 1 == uint256(_idx)) {
                accountActiveAgreementClasses[account].pop();
            } else {
                //swap element and pop
                accountActiveAgreementClasses[account][uint256(_idx)] = accountActiveAgreementClasses[account][_size - 1];
                accountActiveAgreementClasses[account].pop();
            }
        }
    }

    /// review: ok
    function _addAgreementClass(address agreementClass, address account) internal {
        if(_indexOfAgreementClass(agreementClass, account) == -1) {
            accountActiveAgreementClasses[account].push(agreementClass);
        }
    }

    /// @dev Save the balance until now
    /// @param account User to snapshot balance
    function _takeBalanceSnapshot(address account) internal {
        int256 _balance = realtimeBalanceOf(account, block.timestamp) - int256(_balances[account]);
        _settledBalances[account] = _balance < 0 ? 0 : _balance;
    }

    /// @dev Hash agreement with accounts
    function _agreementDataId(address agreementClass, bytes32 agreementId) public pure returns(bytes32) {
        return keccak256(abi.encodePacked(agreementClass, agreementId));
    }
}
