const fs = require("fs");
const RToken = require("@rtoken/contracts/build/contracts/RToken");
const ias = require("@rtoken/contracts/build/contracts/IAllocationStrategy");

fs.mkdir("abis/", err => {
  if (err) {
    // console.error(err)
    return;
  }
  console.log("abis/ directory created");
});
fs.writeFile("abis/RToken.json", JSON.stringify(RToken), err => {
  if (err) {
    console.error(err);
    return;
  }
});
fs.writeFile("abis/IAllocationStrategy.json", JSON.stringify(ias), err => {
  if (err) {
    console.error(err);
    return;
  }
  console.log("abi files updated");
});
