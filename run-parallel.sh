#!/bin/bash
set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <task> <package-list> [<args> ...]"
    exit 1
fi

task="$1"
output_base="runs/$(basename $2)"

package_list="$2"
if [ ! -f "$2" ]; then
    package_list="$(tempfile)"
    echo "$2" > "$package_list"
fi

function _parallel {
    output="$1"
    [ -d "$output" ] || mkdir -p "$output"

    parallel \
        --jobs 100% \
        --bar \
        --tagstring "{}:" \
        --files \
        --result "$output" \
        --joblog "$output /parallel.log" \
        --shuf \
        --timeout 4h \
        -a "$package_list" \
        "$@"
}

function do_run_package {
    _parallel \
        "$output" \
        ./tools/trace-package.R run-package \
        --package "{1}" \
        --decorator "{2}" \
        --type "{3}" \
        --output "$output_base/run-package/output/{1}/{2}/{3}" \
        ::: none noop \
        ::: all
}

function do_trace {
    _parallel \
        "$output" \
        ./tools/trace-package.R trace \
        --package "{1}" \
        --decorator "{2}" \
        --tracer "{3}" \
        --type "{4}" \
        --output "$output_base/trace/output/{1}/{2}/{3}/{4}" \
        ::: count-entry count-exit onexit \
        ::: sequence set \
        ::: all
}

function do_generate {
    _parallel \
        "$output" \
        ./tools/trace-package.R generate \
        --traces "$output_base/trace/output/{1}/onexit/set/all" \
        --output "$output_base/generate/output/{1}/{2}/{3}/{4}"
}

function do_run {
    _parallel \
        "$output" \
        ./tools/trace-package.R run \
        --tests "$output_base/generate/output/{1}/{1}" \
        --output "$output_base/run/output/{1}"
}

function main {
    case "$1" in
        run-package)
            shift
            do_run_package "$@"
        ;;
        trace)
            shift
            do_trace "$@"
        ;;
        generate)
            shift
            do_generate "$@"
        ;;
        run)
            shift
            do_run "$@"
        ;;
        all)
            shift
            do_run_package "$@" \
                && do_trace "$@" \
                && do_generate "$@" \
                && do_run "$@"
        ;;
        *)
            echo "Unknown task $1"
            exit 1
            ;;
    esac
}

main "$@" 2>&1 | tee "run-parallel-$1.log"
