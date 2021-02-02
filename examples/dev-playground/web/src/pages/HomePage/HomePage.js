import { Link, routes } from '@redwoodjs/router'
import UsersCell from 'src/components/UsersCell'
import UsersLayout from 'src/layouts/UsersLayout'

const HomePage = () => {
  return (
    <>
      <div className="sm:text-center lg:text-left">
        <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl">
          <span>Superfluid </span>
          <span className="text-green-600">Developer Playground</span>
        </h1>
        <div className="mt-3 text-base text-gray-500 sm:mt-5 sm:text-lg sm:max-w-xl sm:mx-auto md:mt-5 md:text-xl lg:mx-0">
          <p>Fork this repo to create your own Superfluid dapp</p>
          <p className="mt-3">
            <code>@superfluid-finance/js-sdk</code>
            <br />
            <code>@ethersproject</code>
            <br />
            <code>@redwoodjs</code>
          </p>
        </div>
      </div>
      <div className="mt-6">
        <UsersLayout>
          <UsersCell />
        </UsersLayout>
      </div>
    </>
  )
}

export default HomePage
