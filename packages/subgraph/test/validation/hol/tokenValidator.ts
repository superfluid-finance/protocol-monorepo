import { expect } from "chai";
import { fetchEntityAndEnsureExistence } from "../../helpers/helpers";
import { IToken } from "../../interfaces";
import { getToken } from "../../queries/holQueries";

export const fetchTokenAndValidate = async (
    address: string,
    expectedName: string,
    expectedSymbol: string,
    expectedIsListed: boolean
) => {
    const token = await fetchEntityAndEnsureExistence<IToken>(
        getToken,
        address,
        "Token"
    );

    validateTokenEntity(token, expectedName, expectedSymbol, expectedIsListed);
};

export const validateTokenEntity = (
    subgraphToken: IToken,
    expectedName: string,
    expectedSymbol: string,
    expectedIsListed: boolean
) => {
    expect(subgraphToken.name, "SuperToken: name error").to.be.equal(
        expectedName
    );
    expect(subgraphToken.symbol, "SuperToken: symbol error").to.equal(
        expectedSymbol
    );
    expect(subgraphToken.isListed, "SuperToken: isListed error").to.be.equal(
        expectedIsListed
    );
};
