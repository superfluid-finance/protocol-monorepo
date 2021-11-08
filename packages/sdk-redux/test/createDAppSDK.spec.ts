import {mock} from "jest-mock-extended";
import {createPieces, SuperfluidSource} from "../src";

it('creates SDK-Redux without error', () => {
    const superfluidSource = mock<SuperfluidSource>();

    // Act
    const result = createPieces(superfluidSource);

    // Assert
    expect(result[0]).toBeInstanceOf(superfluidSource);
    expect(result[1]).toBeInstanceOf(Object);
    expect(result[2]).toBeInstanceOf(Object);
});
