module.exports = {
    root: true,
    extends: ["eslint:recommended", "plugin:prettier/recommended"],
    env: {
        node: true,
        es2020: true
    },
    plugins: [
        "prettier",
        "@typescript-eslint"
    ],
    globals: {
        artifacts: "writable"
    },
    rules: {
        "max-len": ["error", 120, { code: 80, ignoreUrls: true }],
        indent: ["error", 4, { SwitchCase: 1 }],
        "linebreak-style": ["error", "unix"],
        quotes: ["error", "double"],
        semi: ["error", "always"],
        "prettier/prettier": "error"
    },
    overrides: [{
        files: ["**/*.ts", "**/*.tsx"],
    }],
};
