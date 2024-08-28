#!/usr/bin/env bash

set -e

if ! which docker >/dev/null 2>&1 ; then
    echo "Please install 'docker' first"
    exit 1
fi

if ! docker compose --help >/dev/null 2>&1; then
    echo "Please install 'docker compose' first"
    exit 1
fi

if ! which jq >/dev/null 2>&1; then
    echo "Please install 'jq' first"
    exit 1
fi

docker compose up --no-start
DOCKER_HOST_IP=$(docker network inspect subgraph_default | jq -r '.[0].IPAM.Config[].Gateway')
export DOCKER_HOST_IP
