task("addStrategy", "Add strategy to manager")
    .addParam("manager", "Manager address")
    .addParam("strategy", "Strategy address")
    .setAction(async (taskArgs, hre) => {
        try {
            console.log("Task: addStrategy");
            const managerAddr = taskArgs.manager;
            const strategyAddr = taskArgs.strategy;
            const manager = await hre.ethers.getContractAt(
                "Manager",
                managerAddr
            );
            const tx = await manager.addApprovedStrategy(strategyAddr);
            console.log(`Tx hash: ${tx.hash}`);
        } catch (error) {
            console.log(error);
        }
    });
