#!/usr/bin/env bash

ci_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

. "${ci_dir}/utils.sh"

log INFO "Building and testing binaries using docker..."

docker build \
    --output="${ci_dir}" \
    -t qlisp_builder \
    -f "${ci_dir}/Dockerfile" \
    "${ci_dir}/.."

if [[ $? -ne 0 ]]; then
    log ERROR "Either tests or build has failed"
    exit 1
else
    log INFO "Done"
fi
