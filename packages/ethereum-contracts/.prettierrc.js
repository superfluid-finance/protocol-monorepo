module.exports = {
    trailingComma: "es5",
    singleQuote: false,
    bracketSpacing: false,
    overrides: [
        {
            files: "*.sol",
            options: {
                bracketSpacing: true,
            },
        },
    ],
};
