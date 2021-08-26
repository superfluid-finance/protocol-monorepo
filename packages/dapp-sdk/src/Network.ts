import { SuperToken } from './superToken';

export interface Network {
    id: number;
    superTokens: Map<string, SuperToken>;
}
