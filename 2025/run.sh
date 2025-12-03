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

    if [[ -f "${day}.sql" ]]; then
        rm -rf .runner.db
        sqlite3 .runner.db \
            'create table input (line TEXT)' \
            '.mode csv' \
            '.headers off' \
            ".import ${input} input"
        time sqlite3 .runner.db -bail < "${day}.sql"
    fi

    if [[ -f "${day}.hs" ]]; then
        run_stack --no-run "${day}.hs"
        time run_stack "${day}.hs" -- +RTS -t -RTS < "${input}"
    fi
}

run_stack() {
    stack script \
        --optimize \
        --use-root \
        --ghc-options=-Wall \
        --resolver=nightly-2025-12-01 \
        --package text \
        "$@"
}

main "$@"
