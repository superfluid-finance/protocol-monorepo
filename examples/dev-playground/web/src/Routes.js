// In this file, all Page components from 'src/pages` are auto-imported. Nested
// directories are supported, and should be uppercase. Each subdirectory will be
// prepended onto the component name.
//
// Examples:
//
// 'src/pages/HomePage/HomePage.js'         -> HomePage
// 'src/pages/Admin/BooksPage/BooksPage.js' -> AdminBooksPage

import { Router, Route } from '@redwoodjs/router'

const Routes = () => {
  return (
    <Router>
      <Route path="/flows/new" page={NewFlowPage} name="newFlow" />
      <Route path="/flows/{id}/edit" page={EditFlowPage} name="editFlow" />
      <Route path="/flows/{id}" page={FlowPage} name="flow" />
      <Route path="/flows" page={FlowsPage} name="flows" />
      <Route path="/" page={HomePage} name="home" />
      <Route path="/users/new" page={NewUserPage} name="newUser" />
      <Route path="/users/{id}/edit" page={EditUserPage} name="editUser" />
      <Route path="/users/{id}" page={UserPage} name="user" />
      <Route path="/users" page={UsersPage} name="users" />
      <Route notfound page={NotFoundPage} />
    </Router>
  )
}

export default Routes
