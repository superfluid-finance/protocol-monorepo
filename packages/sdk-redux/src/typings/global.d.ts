import { SuperfluidContext } from '../SuperfluidContext';

// Solution inspired by: https://stackoverflow.com/a/69429093
declare global {
    var superfluidContext: SuperfluidContext;
}
