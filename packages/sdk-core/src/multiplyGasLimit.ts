import { BigNumber, BigNumberish } from "ethers";

export default function multiplyGasLimit(
    gasLimitish: BigNumberish,
    multiplier: number
): BigNumber {
    const gasLimit = BigNumber.from(gasLimitish);

    // if gasLimit exists, an Overrides object has been passed or the user has explicitly set
    // NOTE: BigNumber doesn't support multiplication with decimals.
    const commonDenominator = 100;
    const multipliedGasLimit =
        multiplier === 1 // No need to modify gas limit when multiplier is 1.
            ? gasLimit
            : gasLimit
                  .div(commonDenominator)
                  .mul(Math.round(multiplier * commonDenominator));

    return multipliedGasLimit;
}
