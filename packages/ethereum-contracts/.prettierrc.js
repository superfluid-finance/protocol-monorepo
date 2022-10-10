module.exports = {
    trailingComma: "es5",
    singleQuote: false,
    bracketSpacing: false,
    overrides: [
        {
            files: "*.sol",
            options: {
                bracketSpacing: true,
                compiler: "0.8.16",
            },
        },
    ],
};
