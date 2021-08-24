import { SuperToken } from './superToken';

export interface Network {
  id: string,
  superTokens: Map<string, SuperToken>,
}
