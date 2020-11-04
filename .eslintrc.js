module.exports = {
    root: true,
    extends: "eslint:recommended",
    env: {
        node: true,
        es2020: true,
    },
    rules: {
        "max-len": ["error", 120, { code: 80, ignoreUrls: true }],
        "indent": ["error", 4],
        "linebreak-style": ["error", "unix"],
        "quotes": ["error", "double"],
        "semi": ["error", "always" ],
    }
};
