import {Framework} from "@superfluid-finance/js-sdk/src/Framework";
import {mock} from "jest-mock-extended";
import {createDAppSDK} from "../src";
import {store} from "../src/store";

it('creates DApp-SDK without error', () => {
    const superfluidSdk = mock<Framework>();

    // Act
    const result = createDAppSDK(superfluidSdk);

    // Assert
    expect(result).toBeInstanceOf(Object);
    expect(result.superfluidSdk).toBe(superfluidSdk);
    expect(result.reduxStore).toBe(store);
});
