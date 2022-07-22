// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

// Hi there! I'm just a little hack so that we can get InstantDistributionAgreementV1.sol into our artifacts directory
// This way, we can easily access and deploy it locally in deploy-sf.js
// Why is this a convenience? Well, InstantDistributionAgreementV1 needs to be deployed with the SlotsBitmapLibrary linked.
// With ethers.getContractFactory, we have the ability to do that, but only if the InstantDistributionAgreementV1.sol is in our artifacts directly
// Phew, so yeah that's why I'm here.

import {InstantDistributionAgreementV1} from "@superfluid-finance/ethereum-contracts/contracts/agreements/InstantDistributionAgreementV1.sol";