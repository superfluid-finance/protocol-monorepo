#!/usr/bin/env bash

# some fixes realted to devcontainer and nix
# Ref: https://github.com/xtruder/nix-devcontainer/issues/12
sudo apt install acl
sudo chmod 1777 /tmp/ && sudo setfacl --remove-default  /tmp
