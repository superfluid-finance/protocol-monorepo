import {mock} from "jest-mock-extended";
import {Framework} from "@superfluid-finance/js-sdk/src/Framework";
import {createDAppSDK} from "../src";

const superfluidSdk = mock<Framework>();
const sut = createDAppSDK(superfluidSdk);

test('subscribe returns error when network not found' , () => {
    // Act
    const result = sut.subscribe(123, "does_not_matter");

    // Assert
    expect(result).rejects.toBe('Network not found.');
});
