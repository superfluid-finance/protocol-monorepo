module.exports = {
    out: "dist/docs",
    exclude: [
        "./src/index.ts",
        "./src/subgraph/**/*.ts",
        "./src/ajvValidations.generated.js",
        "./src/constants.ts",
        "./src/events.ts",
        "./src/frameworkHelpers.ts",
        "./src/mapGetAllEventsQueryEvents.ts",
        "./src/miniSerializeError.ts",
        "./src/ordering.ts",
        "./src/pagination.ts",
        "./src/types.ts",
    ],
};
