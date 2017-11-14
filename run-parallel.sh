#!/bin/bash
set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <package-list>"
    exit 1
fi

package_list="$1"
if [ ! -f "$1" ]; then
    package_list="$(tempfile)"
    echo "$1" > "$package_list"
fi

# TODO: merge with run.sh
parallel \
    --bar \
    --tagstring "{}:" \
    --files \
    --result "runs/packages" \
    --joblog "runs/packages/parallel.log" \
    --shuf \
    --timeout 4h \
    -a "$package_list" \
    ./run.sh "{1}"
