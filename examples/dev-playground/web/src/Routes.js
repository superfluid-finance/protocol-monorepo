// In this file, all Page components from 'src/pages` are auto-imported. Nested
// directories are supported, and should be uppercase. Each subdirectory will be
// prepended onto the component name.
//
// Examples:
//
// 'src/pages/HomePage/HomePage.js'         -> HomePage
// 'src/pages/Admin/BooksPage/BooksPage.js' -> AdminBooksPage

import { Router, Route, Private } from '@redwoodjs/router'

const Routes = ({ useAuth }) => {
  return (
    <Router useAuth={useAuth}>
      <Route path="/flows/{from}/{to}/{token}" page={FlowPage} name="flow" />
      <Route path="/flows" page={FlowsPage} name="flows" />
      <Private unauthenticated="login">
        <Route path="/flows/{from}/{to}/{token}/edit" page={EditFlowPage} name="editFlow" />
        <Route path="/flows/{to}/new" page={NewFlowPage} name="newFlow" />
      </Private>
      <Route path="/" page={HomePage} name="home" />
      <Route path="/login" page={LoginPage} name="login" />
      <Route path="/batchtransfer/{token}" page={BatchTransferPage} name="batchTransfer" />
      <Route path="/users/new" page={NewUserPage} name="newUser" />
      <Route path="/users/{id}/edit" page={EditUserPage} name="editUser" />
      <Route path="/users/{id}" page={UserPage} name="user" />
      <Route path="/users" page={UsersPage} name="users" />
      <Route notfound page={NotFoundPage} />
    </Router>
  )
}

export default Routes
