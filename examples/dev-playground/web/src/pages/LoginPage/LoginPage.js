import { Link, routes, navigate } from '@redwoodjs/router'
import { useAuth } from '@redwoodjs/auth'
import { useParams } from '@redwoodjs/router'

const LoginPage = () => {
  const [isLoggingIn, setIsLoggingIn] = React.useState(false)
  const { logIn, isAuthenticated, loading } = useAuth()
  const { redirectTo } = useParams()

  const onLogIn = async () => {
    setIsLoggingIn(true)
    await logIn()
    setIsLoggingIn(false)
    navigate(redirectTo || routes.home())
  }

  return (
    <>
      <div className="sm:text-center lg:text-left">
        <h1 className="text-l tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl">
          Login
        </h1>
        <p className="mt-4">
          Use the button below to authenticate your account
        </p>
        <button
          disabled={isLoggingIn}
          onClick={onLogIn}
          className="mt-8 whitespace-nowrap inline-flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-blue-600 hover:bg-blue-700"
        >
          {isLoggingIn ? 'Logging in...' : 'Log in'}
        </button>
      </div>
    </>
  )
}

export default LoginPage
