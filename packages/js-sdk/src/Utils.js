/**
 * @dev Utility class
 */
module.exports = class Utils {
    constructor(sf) {
        this._sf = sf;
    }

    normalizeTokenParam(param) {
        return param;
    }

    normalizeAddressParam(param) {
        return param;
    }

    normalizeFlowRateParam(param) {
        return param.toString();
    }
};
