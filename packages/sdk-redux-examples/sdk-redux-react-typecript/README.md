Hi! Welcome to the example project which demonstrated the capabilities of SDK-Redux & SDK-Core.

Run `yarn install` to install all the necessary packages.
If you want to do symlinks (`yarn link`) locally then do it manually or _temporarily_ include this example project in root package.json's "workspaces".
`yarn link-sdks` tries to set up symlinks to `sdk-core` and `sdk-redux` automatically.


## Available Scripts

### `yarn link-sdks`
Tries to set up symlinks to `sdk-core` and `sdk-redux` automatically. Good when developing SDK-s and the example project at the same time.

### `yarn start`

Runs the app in the development mode.<br />
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.<br />
You will also see any lint errors in the console.

### `yarn build`

Builds the app for production to the `build` folder.<br />
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.<br />
Your app is ready to be deployed!
