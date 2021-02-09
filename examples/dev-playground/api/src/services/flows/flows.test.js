import { flows, flow, createFlow, updateFlow, deleteFlow } from './flows'

describe('flows', () => {
  scenario('returns all flows', async (scenario) => {
    const result = await flows()

    expect(result.length).toEqual(Object.keys(scenario.flow).length)
  })

  scenario('returns a single flow', async (scenario) => {
    const result = await flow({ id: scenario.flow.one.id })

    expect(result).toEqual(scenario.flow.one)
  })

  scenario('creates a flow', async (scenario) => {
    const result = await createFlow({
      input: {
        name: 'String',
        body: 'String',
        postId: scenario.flow.one.post.id,
      },
    })

    expect(result.name).toEqual('String')
    expect(result.body).toEqual('String')
  })

  scenario('updates a flow', async (scenario) => {
    const original = await flow({ id: scenario.flow.one.id })
    const result = await updateFlow({
      id: original.id,
      input: { name: 'String6192415' },
    })

    expect(result.name).toEqual('String6192415')
  })

  scenario('deletes a flow', async (scenario) => {
    const original = await deleteFlow({ id: scenario.flow.one.id })
    const result = await flow({ id: original.id })

    expect(result).toEqual(null)
  })
})
