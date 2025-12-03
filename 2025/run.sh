#!/usr/bin/env bash

set -eu -o pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
outdir="${here}/build"

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
        echo -e '\n===== Expected results ====='
        cat "${input}.result"
    fi

    if [[ -f "${day}.c" ]]; then
        echo -e '\n===== C ====='
        local outdir_c="${outdir}/c/${day}"
        mkdir -p "${outdir_c}"
        gcc -Wall -O3 \
            -o "${outdir_c}/main" \
            "${day}.c"
        timer "${outdir_c}/main" < "${input}"
    fi

    if [[ -f "${day}.hs" ]]; then
        echo -e '\n===== Haskell ====='
        local outdir_hs="${outdir}/hs/${day}"
        mkdir -p "${outdir_hs}"
        ghc-9.12 -Wall -O3 \
            -package text \
            -odir "${outdir_hs}" \
            -hidir "${outdir_hs}" \
            -o "${outdir_hs}/Main" \
            "${day}.hs"
        timer "${outdir_hs}/Main" +RTS -t -RTS < "${input}"
    fi

    if [[ -f "${day}.sql" ]]; then
        echo -e '\n===== SQL ====='
        local db=${outdir}/sqlite/${day}.db
        rm -rf "${db}"
        mkdir -p "$(dirname "${db}")"
        sqlite3 "${db}" \
            'create table input (line TEXT)' \
            '.mode csv' \
            '.headers off' \
            ".import ${input} input"
        timer sqlite3 "${db}" -bail < "${day}.sql"
    fi
}

timer() {
    "${here}/timer.py" "$@"
}

main "$@"
