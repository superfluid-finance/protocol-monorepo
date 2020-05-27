/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { IERC20, ISuperToken } from "./interface/ISuperToken.sol";

import "./ERC20Base.sol";
import "./interface/ISuperAgreement.sol";

/**
 * @title Superfluid's token implementation
 * @author Superfluid
 */
contract SuperToken is ISuperToken, ERC20Base {

    /// @dev The underlaying ERC20 token
    IERC20 private _token;

    /// Mapping from sha3(agreementClass, agreementID) to agreement data
    /// the generation of agreementDataID is the logic of agreement contract
    mapping(bytes32 => bytes) private _agreementData;

    /// Mapping from account to agreement state of the account
    /// Mapping order: .agreementClass.account
    /// It is like RUNTIME state of the agreement for each account
    mapping(address => mapping (address => bytes)) private _accountStates;

    /// List of enabled agreement classes for the account
    mapping(address => address[]) private _activeAgreementClasses;

    /// Settled balance for the account
    mapping(address => int256) private _settledBalances;

    constructor (IERC20 token, string memory name, string memory symbol)
    public
    ERC20Base(name, symbol) {
        _token = token;
    }

    /*
     * Account functions
     */

    /// @dev ISuperToken.getAccountActiveAgreements implementation
    function getAccountActiveAgreements(address account)
        public
        override
        view
        returns(address[] memory)
    {
        return _activeAgreementClasses[account];
    }

    /// @dev ERC20.balanceOf implementation
    function balanceOf(
        address account
    )
        public
        view
        override
        returns(uint256 balance)
    {
        (int256 _balance) = _calculateBalance(account, block.timestamp);
        return _balance < 0 ? 0 : uint256(_balance);
    }

    /// @dev ISuperToken.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
        public
        override
        view
        returns (int256)
    {
        return _calculateBalance(account, timestamp);
    }


    /*
    *   Agreement functions
    */

    /// @dev ISuperToken.getAgreementAccountState implementation
    function getAgreementAccountState(
        address agreementClass,
        address account
    )
        external
        view
        override
        returns (bytes memory data)
    {
        return _accountStates[agreementClass][account];
    }

    /// @dev ISuperToken.updateAgreementAccountState implementation
    function updateAgreementAccountState(
        address account,
        bytes calldata state
    )
        external
        override
    {
        // msg.sender is agreementClass
        require(msg.sender != account, "Use the agreement contract");
        _takeBalanceSnapshot(account);
        _accountStates[msg.sender][account] = state;
        state.length != 0 ? _addAgreementClass(msg.sender, account) : _delAgreementClass(msg.sender, account);
        emit AgreementAccountStateUpdated(msg.sender, account);
    }

    /// @dev ISuperToken.createAgreement implementation
    function createAgreement(
        bytes32 id,
        bytes calldata data
    )
        external
        override
    {
        _agreementData[_agreementDataId(msg.sender, id)] = data;
        emit AgreementCreated(msg.sender, id);
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
        bytes32 id
    )
        external
        override
    {
        delete _agreementData[_agreementDataId(msg.sender, id)];
        emit AgreementTerminated(msg.sender, id);
    }

    /*
     * ERC20 compatability functions
     */

    /// @dev ISuperToken.upgrade implementation
    function upgrade(uint256 amount) external override {
        _token.transferFrom(msg.sender, address(this), amount);
        _mint(msg.sender, amount);
        emit TokenUpgraded(msg.sender, amount);
    }

    /// @dev ISuperToken.downgrade implementation
    function downgrade(uint256 amount) external override {
        require(uint256(balanceOf(msg.sender)) >= amount, "amount not allowed");
        //review TODO touch only need, by the requirement amount
        _touch(msg.sender);
        _burn(msg.sender, amount);
        _token.transfer(msg.sender, amount);
        emit TokenDowngraded(msg.sender, amount);
    }

    function getSettledBalance(address account) external view returns(int256 settledBalance) {
        return _settledBalances[account];
    }


    /*
    *  Internal functions
    */

    /* solhint-disable mark-callable-contracts */
    /// @dev Calculate balance as split result if negative return as zero.
    function _calculateBalance(address account, uint256 timestamp) internal view returns(int256) {

        int256 _eachAgreementClassBalance;
        address _agreementClass;

        for (uint256 i = 0; i < _activeAgreementClasses[account].length; i++) {
            _agreementClass = _activeAgreementClasses[account][i];
            _eachAgreementClassBalance +=
                ISuperAgreement(_agreementClass).realtimeBalanceOf(
                    _accountStates[_agreementClass][account],
                    timestamp);
        }

        return _settledBalances[account] + _eachAgreementClassBalance + int256(_balances[account]);
    }
    /* solhint-enable mark-callable-contracts */

    /* solhint-disable mark-callable-contracts */
    /// @notice for each receiving flow, lets set the timestamp to `now`, making a partial settlement
    function _touch(address account) internal {

        address _agreementClass;
        bytes memory _touchState;
        int256 _balance = realtimeBalanceOf(account, block.timestamp) - int256(_balances[account]);

        for (uint256 i = 0; i < _activeAgreementClasses[account].length; i++) {

            _agreementClass = _activeAgreementClasses[account][i];
            _touchState = ISuperAgreement(_agreementClass).touch(
                _accountStates[_agreementClass][account],
                block.timestamp);

            _accountStates[_agreementClass][account] = _touchState;
        }

        _settledBalances[account] = 0;
        if (_balance > 0) {
            _mint(account, uint256(_balance));
        }
    }
    /* solhint-enable mark-callable-contracts */


    /// @dev if returns -1 agreement is not active
    function _indexOfAgreementClass(address agreementClass, address account) internal view returns(int256) {

        int256 i;
        int256 _size = int256(_activeAgreementClasses[account].length);

        while (i < _size) {

            if (_activeAgreementClasses[account][uint256(i)] == agreementClass) {
                return i;
            }

            i++;
        }

        return -1;
    }

    function _delAgreementClass(address agreementClass, address account) internal {

        int256 _idx = _indexOfAgreementClass(agreementClass, account);
        uint256 _size = _activeAgreementClasses[account].length;

        if (_idx >= 0) {

            if (_size - 1 == uint256(_idx)) {
                _activeAgreementClasses[account].pop();
            } else {
                //swap element and pop
                _activeAgreementClasses[account][uint256(_idx)] = _activeAgreementClasses[account][_size - 1];
                _activeAgreementClasses[account].pop();
            }
        }
    }

    /// review: ok
    function _addAgreementClass(address agreementClass, address account) internal {
        if (_indexOfAgreementClass(agreementClass, account) == -1) {
            _activeAgreementClasses[account].push(agreementClass);
        }
    }

    /// @dev Save the balance until now
    /// @param account User to snapshot balance
    function _takeBalanceSnapshot(address account) internal {
        _settledBalances[account] = realtimeBalanceOf(account, block.timestamp) - int256(_balances[account]);
    }

    /// @dev Hash agreement with accounts
    function _agreementDataId(address agreementClass, bytes32 agreementId) private pure returns(bytes32) {
        return keccak256(abi.encodePacked(agreementClass, agreementId));
    }
}
