export const schema = gql`
  type Flow {
    createdAt: DateTime!
    updatedAt: DateTime!
    flowRate: String!
    recipientAddress: String!
    tokenAddress: String!
    ownerAddress: String!
  }

  type Query {
    flows: [Flow!]!
    flow(
      recipientAddress: String!
      ownerAddress: String!
      tokenAddress: String!
    ): Flow
  }
`
