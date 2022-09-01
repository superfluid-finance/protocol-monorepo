module.exports = {
    trailingComma: "es5",
    singleQuote: false,
    bracketSpacing: false,
    overrides: [
        {
            files: "*.sol",
            options: {
                printWidth: 80,
                bracketSpacing: true,
                compiler: "0.8.14"
            },
        },
    ],
};
