import { gql } from "graphql-request";

export const getSuperTokens = gql`
    {
        response: tokens(where: { isSuperToken: true }) {
            id
            createdAtTimestamp
            createdAtBlockNumber
            name
            symbol
            isListed
            underlyingAddress
        }
    }
`;
