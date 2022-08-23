#!/bin/sh

git submodule update --init --recursive "$1"
git submodule sync --recursive "$1"
