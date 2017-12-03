#!/bin/bash
set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <package>"
    exit 1
fi

package="$1"
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

function run_task {
    name="$1"
    shift

    output="$output_base/$name"

    if [ ! -d "$output" ]; then
        echo "running task $name..."

        _parallel \
            "$output" \
            $@
    else
        echo "task $name already done, skipping"
    fi
}

function trace_task {
    config="$1"
    name="trace-$config"

    run_task \
        "$name" \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config "$config" \
        --type "{1}" \
        --output "$output_base/$name/output/{1}" \
        ::: all
}

# run_task \
#    "run-package" \
#    ./tools/trace-package.R trace \
#    --package "$package" \
#    --config "{1}" \
#    --type "{2}" \
#    --output "$output_base/run-package/output/{1}/{2}" \
#    ::: none \
#    ::: all

#trace_task count-entry--sequence
#trace_task count-exit--sequence
#trace_task onexit--sequence
#trace_task on.exit--sequence
trace_task on.exit--set

# run_task \
#     "generate" \
#     ./tools/trace-package.R generate \
#     --traces "$output_base/trace/output/on.exit--set/{1}" \
#     --output "$output_base/generate/output" \
#     ::: all

# run_task \
#     "run" \
#     ./tools/trace-package.R run \
#     --tests "$output_base/generate/{1}" \
#     --output "$output_base/run/output" \
#     ::: output
