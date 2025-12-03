#!/usr/bin/env bash

set -eu -o pipefail

main() {
    local input=${1:-}; shift

    if [[ -z "${input}" ]]; then
        echo >&2 'Input file is required'
        exit 1
    fi

    local day=$(basename "${input}" | sed 's/.txt//')
    if [[ "${day}" == *-sample ]]; then
        day="${day%%-sample}"
    fi

    run_stack --no-run "${day}.hs"
    time run_stack "${day}.hs" -- +RTS -t -RTS < "${input}"
}

run_stack() {
    stack script \
        --optimize \
        --use-root \
        --ghc-options=-Wall \
        --ghc-options=-Werror \
        --resolver=nightly-2025-12-01 \
        --package text \
        "$@"
}

main "$@"
