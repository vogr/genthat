#!/bin/bash -x
set -e

FORCE=${FORCE:-}
WORKER_JOBS=${WORKER_JOBS:-1}

if [ -z $GENTHAT_SOURCE_PATHS ]; then
   echo "Missing GENTHAT_SOURCE_PATHS"
   exit 1
fi

if [ $# -eq 1 ]; then
#    tasks="--coverage"
#    tasks="--run-generated-tests"
    tasks="--covr-revdep"
#    tasks="--revdep"
#    tasks="--trace --coverage"
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
#        exit 1
    else
        mkdir -p "$output"
    fi

    parallel \
        --bar \
        --jobs $WORKER_JOBS \
        --tagstring "{}:" \
        --files \
        --result "$output" \
        --joblog "$output/parallel.log" \
        --shuf \
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

#    if [ ! -d "$output" ]; then
        echo "running task $name..."

        _parallel \
            "$output" \
            $@
#    else
#        echo "task $name already done, skipping"
#    fi
}

function do_trace_task {
    config="$1"
    shift
    name="trace-$config"

    do_run_task \
        "$name" \
        --timeout 2h \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config "$config" \
        --type "{1}" \
        --output "$output_base/$name/output/{1}" \
        "$@" \
        ::: all
}

function trace_task {
#    do_trace_task count-entry--sequence --action stats
#    do_trace_task count-exit--sequence --action
#    do_trace_task onexit--sequence --action stats
#    do_trace_task onexit--set --action stats
#    do_trace_task on.exit--sequence --action stats
#    do_trace_task count-entry--set --action stats
    do_trace_task on.exit--set --action generate --prune-tests
}

function revdep_task {
    deps=$(mktemp)
    Rscript -e "options(repos='https://mirrors.nic.cz/R'); cat(paste(intersect(installed.packages()[,1], tools::package_dependencies('$package', reverse=T, recursive=F)[[1]]), collapse='\n'),'\n')" > $deps

    do_run_task \
        "revdep" \
        --timeout 1h \
        -a "$deps" \
        ./tools/trace-package.R revdep \
        --package "$package" \
        --dep "{1}" \
        --output "$output_base/revdep/output/all/{1}"
}

function covr_revdep_task {
    deps=$(mktemp)
    Rscript -e "options(repos='https://mirrors.nic.cz/R'); cat(paste(intersect(installed.packages()[,1], tools::package_dependencies('$package', reverse=T, recursive=F)[[1]]), collapse='\n'),'\n')" > $deps

    do_run_task \
        "covr-revdep" \
        --timeout 1h \
        -a "$deps" \
        ./tools/trace-package.R covr-revdep \
        --package "$package" \
        --dep "{1}" \
        --output "$output_base/covr-revdep/output/all/{1}"
}

function run_generated_tests_task {
    do_run_task \
        "run-generated-tests" \
        --timeout 1h \
        ./tools/trace-package.R run-generated-tests \
        --package "$package" \
        --tests "$output_base/trace-on.exit--set/output/all" \
        --output "$output_base/run-generated-tests/output/{1}" \
        ::: all
}

function run_package_task {
    do_run_task \
        "run-package" \
        --timeout 1h \
        ./tools/trace-package.R trace \
        --package "$package" \
        --config none \
        --type "{1}" \
        --output "$output_base/run-package/output/{1}" \
        ::: all
}

function run_task {
    do_run_task \
        "run" \
        ./tools/trace-package.R run \
        --tests "$output_base/trace-on.exit--set/output/{1}/genthat-tracing.csv" \
        --output "$output_base/run/output/{1}" \
        ::: all
}

function coverage_task {
    do_run_task \
        "coverage" \
        ./tools/trace-package.R coverage \
        --package "$package" \
        --types "{1}" \
        --output "$output_base/coverage/output/{1}" \
        ::: all tests
}

for t in $(echo "$tasks"); do
    case "$t" in
        --run-package)
            run_package_task
            ;;
        --trace)
            trace_task
            ;;
        --revdep)
            revdep_task
            ;;
        --covr-revdep)
            covr_revdep_task
            ;;
        --run-generated-tests)
            run_generated_tests_task
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
