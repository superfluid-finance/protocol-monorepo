// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

import { Ownable } from "../access/Ownable.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance
} from "../interfaces/superfluid/ISuperfluid.sol";

contract TestGovernance is
    Ownable,
    ISuperfluidGovernance
{
    address private _rewardAddress;
    uint256 private _liquidationPeriod;
    address[] private _agreementList;
    mapping (address => uint) private _agreementMap;

    constructor(
        address rewardAddress,
        uint256 liquidationPeriod
    )
    {
        _owner = msg.sender;
        _rewardAddress = rewardAddress;
        _liquidationPeriod = liquidationPeriod;
    }

    function getRewardAddress(
        address /* superToken */
    )
        external
        view
        override
        returns(address rewardAddress)
    {
        return _rewardAddress;
    }

    function setRewardAddress(
        address rewardAddress
    )
        external
        onlyOwner
    {
        _rewardAddress = rewardAddress;
    }

    function getLiquidationPeriod(
        address /* superToken */
    )
        external
        view
        override
        returns(uint256 period)
    {
        return _liquidationPeriod;
    }

    function addAgreement(address agreementClass) external onlyOwner override {
        require(agreementClass != address(0), "TestGovernance: 0x address detected");
        _agreementList.push(agreementClass);
        _agreementMap[agreementClass] = _agreementList.length;
    }

    function isAgreementListed(address agreementClass) external override view returns(bool yes) {
        return _agreementMap[agreementClass] > 0;
    }

    function mapAgreements(uint256 bitmap)
        external view override
        returns (address[] memory agreementClasses) {
        uint i;
        uint n;
        // create memory output using the counted size
        agreementClasses = new address[](_agreementList.length);
        // add to the output
        n = 0;
        for (i = 0; i < _agreementList.length; ++i) {
            if ((bitmap & (1 << i)) > 0) {
                agreementClasses[n++] = _agreementList[i];
            }
        }
        // resize memory arrays
        assembly {
            mstore(agreementClasses, n)
        }
    }

    function maskAgreementBit(uint256 bitmap, address agreementClass)
        external view override
        returns (uint256 newBitmap)
    {
        require(_agreementMap[agreementClass] > 0, "TestGovernance: Agreement not listed");
        return bitmap | (1 << (_agreementMap[agreementClass] - 1));
    }

    function unmaskAgreementBit(uint256 bitmap, address agreementClass)
        external view override
        returns (uint256 newBitmap)
    {
        require(_agreementMap[agreementClass] > 0, "TestGovernance: Agreement not listed");
        return bitmap & ~(1 << (_agreementMap[agreementClass] - 1));
    }
}
