import { Token } from './token';

export interface Chain {
  id: string,
  tokens: Map<string, Token>,
}
