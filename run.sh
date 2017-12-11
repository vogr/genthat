#!/bin/bash
set -e

FORCE=${FORCE:-}

if [ $# -eq 1 ]; then
    tasks="--trace --generate --run --coverage"
#    tasks="--run-package --trace --generate --run --coverage"
    package="$1"
elif [ $# -gt 1 ]; then
    tasks=""
    while [ $# -gt 1 ]; do tasks="$tasks $1"; shift; done
    package="$1"
else
    echo "Usage: $0 [TASKS] <package>"
    exit 1
fi

output_base="runs/packages/$package"

function _parallel {
    output="$1"
    shift

    if [ -d "$output" ]; then
        echo "Output $output exists!"
        exit 1
    else
        mkdir -p "$output"
    fi

    parallel \
        --bar \
        --jobs 1 \
        --tagstring "{}:" \
        --files \
        --result "$output" \
        --joblog "$output/parallel.log" \
        --shuf \
        --timeout 5h \
        "$@"
}

function do_run_task {
    name="$1"
    shift

    output="$output_base/$name"

    if [ ! -z "$FORCE" ]; then
        echo "removing existing output: $output"
        rm -fr "$output"
    fi

    if [ ! -d "$output" ]; then
        echo "running task $name..."

        _parallel \
            "$output" \
            $@
    else
        echo "task $name already done, skipping"
    fi
}

function do_trace_task {
    config="$1"
    shift
    name="trace-$config"

    do_run_task \
        "$name" \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config "$config" \
        --type "{1}" \
        --output "$output_base/$name/output/{1}" \
        "$@" \
        ::: all
}

function trace_task {
    do_trace_task count-entry--sequence --discard-traces
    #do_trace_task count-exit--sequence --discard-traces
    #do_trace_task onexit--sequence --discard-traces
    #do_trace_task onexit--set --discard-traces
    #do_trace_task on.exit--sequence --discard-traces
    do_trace_task on.exit--set
}

function run_package_task {
    do_run_task \
        "run-package" \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config none \
        --type "{1}" \
        --output "$output_base/run-package/output/{1}" \
        ::: all
}

function generate_task {
    do_run_task \
        "generate" \
        ./tools/trace-package.R generate \
        --traces "$output_base/trace-on.exit--set/output/{1}/genthat-traces.csv" \
        --output "$output_base/generate/output/{1}" \
        ::: all
}

function run_task {
    do_run_task \
        "run" \
        ./tools/trace-package.R run \
        --tests "$output_base/generate/output/{1}/genthat-tests.csv" \
        --output "$output_base/run/output/{1}" \
        ::: all
}

function coverage_task {
    do_run_task \
        "coverage" \
        ./tools/trace-package.R coverage \
        --package "$package" \
        --output "$output_base/coverage/output/{1}" \
        ::: all
}

for t in $(echo "$tasks"); do
    case "$t" in
        --run-package)
            run_package_task
            ;;
        --trace)
            trace_task
            ;;
        --generate)
            generate_task
            ;;
        --run)
            run_task
            ;;
        --coverage)
            coverage_task
            ;;
        *)
            echo "Unknown task $t"
            exit 1
            ;;
    esac
done
