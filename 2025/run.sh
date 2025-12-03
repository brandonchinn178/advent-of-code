#!/usr/bin/env bash

set -eu -o pipefail

outdir=build

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

    if [[ -f "${input}.result" ]]; then
        echo '===== Expected results ====='
        cat "${input}.result"
        echo ''
    fi

    if [[ -f "${day}.sql" ]]; then
        local db=${outdir}/sqlite/${day}.db
        rm -rf "${db}"
        mkdir -p "$(dirname "${db}")"
        sqlite3 "${db}" \
            'create table input (line TEXT)' \
            '.mode csv' \
            '.headers off' \
            ".import ${input} input"
        time sqlite3 "${db}" -bail < "${day}.sql"
    fi

    if [[ -f "${day}.hs" ]]; then
        local outdir_hs="${outdir}/hs/${day}"
        mkdir -p "${outdir_hs}"
        ghc-9.12 -Wall -O3 \
            -package text \
            -odir "${outdir_hs}" \
            -hidir "${outdir_hs}" \
            -o "${outdir_hs}/Main" \
            "${day}.hs"
        time "${outdir_hs}/Main" +RTS -t -RTS < "${input}"
    fi
}

main "$@"
