import { AuthenticationError } from '@redwoodjs/api'

import { bufferToHex } from 'ethereumjs-util'
import { recoverPersonalSignature } from 'eth-sig-util'
import jwt from 'jsonwebtoken'

import { db } from 'src/lib/db'

const NONCE_MESSAGE =
  'Please prove you control this wallet by signing this random text: '

const getNonceMessage = (nonce) => NONCE_MESSAGE + nonce

export const authChallenge = async ({ input: { address: addressRaw } }) => {
  const nonce = Math.floor(Math.random() * 1000000).toString()
  const address = addressRaw.toLowerCase()
  await db.user.upsert({
    where: { address },
    update: {
      authDetail: {
        update: {
          nonce,
          timestamp: new Date(),
        },
      },
    },
    create: {
      address,
      authDetail: {
        create: {
          nonce,
        },
      },
    },
  })

  return { message: getNonceMessage(nonce) }
}

export const authVerify = async ({
  input: { signature, address: addressRaw },
}) => {
  try {
    const address = addressRaw.toLowerCase()
    const authDetails = await db.user
      .findFirst({
        where: { address },
      })
      .authDetail()
    if (!authDetails) throw new Error('No authentication started')

    const { nonce, timestamp } = authDetails
    const startTime = new Date(timestamp)
    if (new Date() - startTime > 5 * 60 * 1000)
      throw new Error(
        'The challenge must have been generated within the last 5 minutes'
      )
    const signerAddress = recoverPersonalSignature({
      data: bufferToHex(Buffer.from(getNonceMessage(nonce), 'utf8')),
      sig: signature,
    })
    if (address !== signerAddress.toLowerCase())
      throw new Error('invalid signature')

    const token = jwt.sign({ address }, process.env.ETHEREUM_JWT_SECRET, {
      expiresIn: '5h',
    })
    return { token }
  } catch (e) {
    throw new Error(e)
  }
}
