import { initializeTestEnvironment } from "./TestEnvironment";

before(async () => {
    console.log("Initializing test environment...");
    await initializeTestEnvironment();
    console.log("Test environment initialized!");
});
