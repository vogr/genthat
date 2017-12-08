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

function do_run_task {
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
    batch_size="$2"
    name="trace-$config"

    do_run_task \
        "$name" \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config "$config" \
        --type "{1}" \
        --output "$output_base/$name/output/{1}" \
        --batch-size "$batch_size" \
        ::: all
}

function run_package_task {
    do_run_task \
        "run-package" \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config "{1}" \
        --type "{2}" \
        --output "$output_base/run-package/output/{1}/{2}" \
        ::: none \
        ::: all
}

function generate_task {
    do_run_task \
        "generate" \
        ./tools/trace-package.R generate \
        --traces "$output_base/trace-on.exit--set/output/{1}" \
        --output "$output_base/generate/output" \
        ::: all
}

function run_task {
    do_run_task \
        "run" \
        ./tools/trace-package.R run \
        --tests "$output_base/generate/{1}" \
        --output "$output_base/run/output" \
        ::: output
}

function coverage_task {
    do_run_task \
        "coverage" \
        ./tools/trace-package.R coverage \
        --package "$package" \
        --output "$output_base/coverage/output/{1}" \
        ::: all
}

run_package_task
trace_task count-entry--sequence -1
#trace_task count-exit--sequence -1
trace_task onexit--sequence -1
#trace_task onexit--set -1
trace_task on.exit--sequence -1
trace_task on.exit--set 0
generate_task
run_task
coverage_task

