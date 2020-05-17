module.exports = {
    root: true,
    extends: "eslint:recommended",
    env: {
        node: true,
        es2017: true
    },
    rules: {
        //"max-len": ["error", 120, 4],
        "indent": ["error", 4],
        "linebreak-style": ["error", "unix"],
        "quotes": ["error", "double"],
        "semi": ["error", "always" ],
    }
};
