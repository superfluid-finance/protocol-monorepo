#!/bin/bash

# flattens hardhat build output and formats it into what truffle would output
tasks/flatten-folder.sh build/contracts '*.dbg.json' ./build/contracts 
tasks/flatten-folder.sh build/@openzeppelin/contracts '*.dbg.json' ./build/contracts 
tasks/hardhat-build-cleanup.sh build/contracts