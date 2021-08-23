<h1 align="center">Welcome to SF Developer Playground 👋</h1>
<p>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid\_HQ" target="_blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>

> A forkable Superfluid Dashboard which uses @superfluid-finance/js-sdk

### 🏠 [Homepage](https://superfluid.finance)

## Install

#### Step 1.

copy `.env.template` to `.env` and add the required variables. You can use this to generate a string for `ETHEREUM_JWT_SECRET`:

```bash
openssl rand -base64 48
```

#### Step 2.

```sh
yarn install

yarn prisma migrate save --experimental --schema "./api/db/schema.prisma"
yarn prisma migrate up --experimental --schema "./api/db/schema.prisma"
```

## Usage

```sh
yarn rw dev
```

Now you can flow tokens to another user. Click their address and hit "Flow" to get started!

## New to Redwood?

- [Tutorial](https://redwoodjs.com/tutorial/welcome-to-redwood): getting started and complete overview guide.
- [Docs](https://redwoodjs.com/docs/introduction): using the Redwood Router, handling assets and files, list of command-line tools, and more.
- [Redwood Community](https://community.redwoodjs.com): get help, share tips and tricks, and collaborate on everything about RedwoodJS.

## Hosting

If you get into trouble during hosting, due to the api serverless bundle size, you can use these commands to test out your bundle size

```bash
cd api
yarn rw build api
node zip-it.js
# outputs to graphql.zip
```

### Ngrok it

```bash
# In VM
yarn rw dev --fwd="--host=0.0.0.0"

# on host machine
./ngrok http 8910 --host-header=rewrite
```

## Author

👤 **Patrick Gallagher**

- Website: https://patrickgallagher.dev
  - Twitter: [@Superfluid_HQ](https://twitter.com/pi0neerpat)
  - GitHub: [@pi0neerpat](https://github.com/pi0neerpat)

## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
