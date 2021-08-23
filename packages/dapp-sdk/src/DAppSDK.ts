import { Framework } from '@superfluid-finance/js-sdk/src/Framework';

import { store } from './store';

// TODO: Should we constrain to certain tokens?
export class DAppSDK {
  constructor(public readonly superfluidJsSdk: Framework) { // TODO: Abstract away browser storage.
    this.reduxStore = store;
  }

  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  // eslint-disable-next-line @typescript-eslint/no-unused-vars,@typescript-eslint/no-empty-function
  public subscribe(chainId: string, accountAddress: string) {

  }

  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  private readonly reduxStore;
}
