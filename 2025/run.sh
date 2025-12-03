#!/usr/bin/env bash

set -eu -o pipefail

main() {
    local input=${1:-}; shift

    if [[ -z "${input}" ]]; then
        echo >&2 'Day is not provided'
        exit 1
    fi

    if [[ "${input}" == *-sample ]]; then
        local day="${input%%-sample}"
    else
        local day="${input}"
    fi

    run_stack --no-run "${day}.hs"
    time run_stack "${day}.hs" < "data/${input}.txt"
}

run_stack() {
    stack script \
        --resolver=nightly-2025-12-01 \
        --optimize \
        --use-root \
        --ghc-options=-Wall \
        --ghc-options=-Werror \
        "$@"
}

main "$@"
